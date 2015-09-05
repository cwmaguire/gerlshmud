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
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

id(_Props, Owner, Pid) ->
    "hp_of_" ++ Owner ++ "_" ++ Pid.

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Owner, Props, {damage, _Att, _Src, Owner, _Dmg}) ->
    {succeed, true, Props};
attempt(_Owner, Props, _Msg) ->
    {succeed, false, Props}.

succeed(Props, {damage, Attack, Source, Owner, Damage}) ->
    take_damage(Attack, Source, Owner, Damage, Props);
succeed(Props, _Msg) ->
    %log("saw ~p succeed with props ~p~n", [Msg, Props]),
    Props.

fail(Props, Message, _Reason) ->
    log([<<"saw ">>, Message, <<" fail with props ">>, Props]),
    Props.

take_damage(Attack, Source, Owner, Damage, Props) ->
    %log("took ~p damage~nProps: ~p~n", [Damage, Props]),
    Hp = proplists:get_value(hitpoints, Props, 0) - Damage,
    case Hp of
        X when X < 1 ->
            log([<<"dying">>]),
            Owner = proplists:get_value(owner, Props),
            erlmud_object:attempt(Owner, {killed, Attack, Source, Owner});
        _ ->
            ok
    end,
    lists:keystore(hitpoints, 1, Props, {hitpoints, Hp}).

log(Terms) ->
    erlmud_event_log:log([atom_to_list(?MODULE) | Terms]).
