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
-module(erlmud_behaviour).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Owner, Props, Msg = {damage, _Att, _Src, Owner, _Dmg}) ->
    log("subscribed attempt: ~p, props: ~p~n", [Msg, Props]),
    {succeed, true, Props};
attempt(Owner, Props, Msg = {attack, _Att, Owner, _Target}) ->
    log("subscribed attempt: ~p,~nprops: ~p~n", [Msg, Props]),
    {succeed, true, Props};
attempt(Owner, Props, {stop_attack, Attack, Owner, _Target}) ->
    case [Pid || {attack, Pid, _} <- Props, Pid == Attack] of
        [_ | _] ->
            lists:keydelete(attack, 1, Props);
        _ ->
            Props
    end;
attempt(Owner, Props, Msg) ->
    log("ignoring attempt: ~p, props: ~p~nowner: ~p~n", [Msg, Props, Owner]),
    {succeed, false, Props}.

succeed(Props, {attack, Attack, Attacker, Target}) ->
    case proplists:get_value(owner, Props) of
        Owner when Owner == Attacker ->
            lists:keystore(attack, 1, Props, {attack, Attack, Target});
        _ ->
            Props
    end;
succeed(Props, {damage, _Att, Attacker, Owner, _Dmg}) ->
    log("caught damage succeeded~n~p~n~p~n", [Attacker, Owner]),
    _ = case [Target || {attack, _, Target} <- Props, Target == Attacker] of
        [] ->
            log("no attacks on ~p~nprops:~n\t~p~n", [Attacker, Props]),
            AttackWait = proplists:get_value(attack_wait, Props, 1000),
            erlmud_object:attempt_after(AttackWait,
                                        Owner,
                                        {attack, Owner, Attacker});
        _ ->
            log("already attacks on ~p~nprops:~n\t~p~n", [Attacker, Props]),
            ok
    end,
    Props;
succeed(Props, {stop_attack, Pid, Source, _Target}) ->
    Owner = proplists:get_value(owner, Props),
    case Owner == Source andalso
         [Attack || {attack, Attack, _} <- Props, Attack == Pid] of
        [_ | _] ->
            lists:keydelete(attack, 1, Props);
        _ ->
            Props
    end;
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    Props.

fail(Props, Result, Msg) ->
    log("result: ~p message: ~p~n", [Result, Msg]),
    Props.

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
