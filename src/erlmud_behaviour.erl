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
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

id(_Props, Owner, Pid) ->
    "behaviour_for_" ++ Owner ++ "_" ++ Pid.

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Owner, Props, {damage, _Att, _Src, Owner, _Dmg}) ->
    {succeed, true, Props};
attempt(Owner, Props, {attack, _Att, Owner, _Target}) ->
    {succeed, true, Props};
attempt(Owner, Props, {stop_attack, Attack, Owner, _Target}) ->
    case [Pid || {attack, Pid, _} <- Props, Pid == Attack] of
        [_ | _] ->
            lists:keydelete(attack, 1, Props);
        _ ->
            Props
    end;
attempt(_, Props, _) ->
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
    log([<<"caught damage succeeded ">>, Attacker, <<" ">>, Owner]),

    %% pitbull attack: stick with first character that damages us
    %% TODO: make sure the attack originates from something we can attack back,
    %%       not a poison or extreme cold or something.
    _ = case [Attack || Attack = {attack, _, _} <- Props] of
        [] ->
            log([<<"no attacks yet, attack back props: ">>, Props]),
            AttackWait = proplists:get_value(attack_wait, Props, 1000),
            erlmud_object:attempt_after(AttackWait,
                                        Owner,
                                        {attack, Owner, Attacker});
        _ ->
            log([<<"already attacking something, stick with it props: ">>, Props]),
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
    log([<<"saw ">>, Msg, <<" succeed with props ">>, Props]),
    Props.

fail(Props, Result, Msg) ->
    log([<<"result: ">>, Result, <<" message: ">>, Msg]),
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
