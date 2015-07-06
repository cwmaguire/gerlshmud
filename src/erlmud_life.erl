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
-module(erlmud_life).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).
-export([died/3]).

added(_, _) -> ok.
removed(_, _) -> ok.
died(_, _, _) -> ok.

is_dead_action(revive) ->
    true;
is_dead_action(_) ->
    false.

attempt(Owner, Props, {die, Target}) when Owner == Target ->
    {succeed, _Subscribe = true, Props};
attempt(Owner, Props, Msg) when Owner == element(2, Msg) ->
    IsAlive = proplists:get_value(is_alive, Props, false),
    IsDeadAction = is_dead_action(element(1, Props)),
    case IsAlive orelse IsDeadAction of
        true ->
            {succeed, _Subscribe = false, Props};
        false ->
            {fail, _Subscribe = false, Props}
    end;
attempt(Owner, Props, {calc_hit, Attack, Attacker, Owner, _}) ->
    case proplists:get_value(is_alive, Props) of
        false ->
            {{resend, Attacker, {killed, Attack, Attacker, Owner}}, false, Props};
        _ ->
            {succeed, false, Props}
    end;
attempt(_Owner, Props, _Msg) ->
    {succeed, false, Props}.

succeed(Props, {die, Target}) ->
    Owner = proplists:get_value(owner, Props),
    case Owner == Target of
        true ->
            CorpseCleanupMilis = application:get_env(erlmud, corpse_cleanup_milis, 10 * 60 * 1000),
            erlmud_object:attempt_after(CorpseCleanupMilis, self(), {cleanup, Owner}),
            lists:keystore(is_alive, 1, Props, {is_alive, false});
        _ ->
            Props
    end;
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    throw(should_never_happen).
    %Props.

fail(Props, Message, _Reason) ->
    log("saw ~p fail with props ~p~n", [Message, Props]),
    throw(should_never_happen).
    %Props.

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
