%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(erlmud_hitpoints).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Owner, Props, {damage, _Att, _Src, Owner, _Dmg}) ->
    {succeed, true, Props};
attempt(_Owner, Props, _Msg) ->
    {succeed, false, Props}.

succeed(Props, {damage, _Attack, _Source, Target, Damage}) ->
    Owner = proplists:get_value(owner, Props),
    case Owner == Target of
        true ->
            take_damage(Damage, Props);
        _ ->
            Props
    end;
succeed(Props, {die, _}) ->
    Props;
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    %throw(should_never_happen).
    Props.

% What was this for? Wouldn't match since Message is a string
%fail(Props, {damage, _, _, _, _} = Message, _Reason) ->
    %log("saw ~p fail with props ~p~n", [Message, Props]);
fail(Props, Message, _Reason) ->
    log("saw ~p fail with props ~p~n", [Message, Props]),
    %throw(should_never_happen).
    Props.

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).

take_damage(Damage, Props) ->
    log("took ~p damage~nProps: ~p~n", [Damage, Props]),
    Hp = proplists:get_value(hitpoints, Props, 0) - Damage,
    case Hp of
        X when X < 1 ->
            Owner = proplists:get_value(owner, Props),
            erlmud_object:attempt(Owner, {die, Owner});
        _ ->
            ok
    end,
    lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}).
