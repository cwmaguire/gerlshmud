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
-module(erlmud_handler_counterattack).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Owner, Props, {damage, _Att, _Src, Self, _Dmg}}) when Self == self() ->
    log([<<"caught damager attempt">>]),
    {succeed, true, Props};
attempt({_Owner, Props, {attack, _Att, Self, _Target}}) when Self == self() ->
    log([<<"caught attack attempt">>]),
    {succeed, true, Props};
attempt({_Owner, Props, {stop_attack, Attack, Self, _Target}}) when Self == self() ->
    log([<<"caught stop_attack attempt">>]),
    case [Pid || {attack, Pid, _} <- Props, Pid == Attack] of
        [_ | _] ->
            lists:keydelete(attack, 1, Props);
        _ ->
            Props
    end;
attempt({_Owner, _Props, _Attempt}) ->
    %log([self(), <<" caught attempt but not subscribing">>, Attempt]),
    undefined.

%succeed({Props, {attack, Attack, Self, Target}}) when Self == self() ->
    %% I'm guessing this replaces the {attack, AttackPid} property
    %% that erlmud_handler_char_attack adds
    %% (previously erlmud_character)
    %% No, this was it's own process so it had it's own
    %% properties.
    %lists:keystore(attack, 1, Props, {attack, {Attack, Target}});
%% should we always counter-attack the most recent attacker?
%% That should be an option for the particular behaviour to decide:
%% a particularly tenacious enemy will stick to one attacker; a less decisive
%% enemy might keep switching to attack the most recent thing that attacked it.
%% (e.g. something stupid, or with a short memory)
succeed({Props, {damage, _Att, Attacker, Self, _Dmg}}) when Self == self() ->
    %log([<<"caught damage succeeded ">>]),

    %% pitbull attack: stick with first character that damages us
    %% TODO: make sure the attack originates from something we can attack back,
    %%       not a poison or extreme cold or something.
    Attack = proplists:get_value(attack, Props),
    Target = proplists:get_value(target, Props),
    _ = case is_pid(Attack) andalso is_pid(Target) of
        false ->
            log([<<"no attacks yet, attack back props: ">>, Props]),
            AttackWait = proplists:get_value(attack_wait, Props, 1000),
            erlmud_object:attempt_after(AttackWait,
                                        self(),
                                        {attack, self(), Attacker});
        true ->
            log([<<"already attacking ">>, Target, <<" with ">>, Attack, <<". Stick with it. Props: ">>, Props]),
            ok
    end,
    Props;
succeed({Props, _}) ->
    %log([<<"Counterattack saw some success">>]),
    Props.

fail({Props, Result, Msg}) ->
    log([<<"result: ">>, Result, <<" message: ">>, Msg]),
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
