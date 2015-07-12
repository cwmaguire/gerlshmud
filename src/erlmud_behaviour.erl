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
%% should we always counter-attack the most recent attacker?
%% That should be an option for the particular behaviour to decide:
%% a particularly tenacious enemy will stick to one attacker; a less decisive
%% enemy might keep switching to attack the most recent thing that attacked it.
%% (e.g. something stupid, or with a short memory)
succeed(Props, {damage, _Att, Attacker, Owner, _Dmg}) ->
    log("caught damage succeeded~n~p~n~p~n", [Attacker, Owner]),

    %% pitbull attack: stick with first character that damages us
    %% TODO: make sure the attack originates from something we can attack back,
    %%       not a poison or extreme cold or something.
    _ = case [Attack || Attack = {attack, _, _} <- Props] of
        [] ->
            log("no attacks yet, attack back~nprops:~n\t~p~n", [Props]),
            AttackWait = proplists:get_value(attack_wait, Props, 1000),
            erlmud_object:attempt_after(AttackWait,
                                        Owner,
                                        {attack, Owner, Attacker});
        _ ->
            log("already attacking something, stick with it~nprops:~n\t~p~n", [Props]),
            ok
    end,
    Props;
%% If we have an attack that stopped, remove it
succeed(Props, {stop_attack, AttackPid}) ->
    case [Attack || {attack, Attack, _} <- Props, Attack == AttackPid] of
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
