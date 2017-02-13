%% Copyright (c) 2016, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_handler_attack_resource).
-behaviour(erlmud_handler).

%% respond to resources being added to the owner by reserving
%% those resources and kicking off attacks
%%
%% 1) subscribe to resource increase
%% 2) on resource increase success kick off resource reservation
%% 3) on resource reservation success allocate resources
%% 4) if any attack has all the necessary resources then kick off attack

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

attempt({_Owner, Props, {add, _N, dunno}})
  when is_pid(Attack#attack.target),
       is_list(AttackTypes) ->
    Character = proplists:get_value(character, Props),
    AttackType = proplists:get_value(attack_type, Props),
    DoesAttackTypeMatch = lists:member(AttackType, AttackTypes),
    case {Attack#attack.source, DoesAttackTypeMatch} of
        {Character, true} ->
            {succeed, true, Props};
        _ ->
            {succeed, false, Props}
    end;
%% TODO some items will have to be worn or wielded in order to
%% take effect. Add a function or property that determines if the
%% item's modifiers apply.
%attempt({_Owner, Props, {Source, {attack, Attack}, Target, with, Self, CalcType, Value}})

%% calculate either hit, damage or wait for an attack in progress
%% with this item
attempt({_Owner, Props, Attack = #attack{source = Source,
                                         target = Target,
                                         weapon = Self,
                                         calc_type = CalcType}})
  when Self == self() andalso
       (CalcType == hit orelse CalcType == damage) ->
    log(debug, [<<"Saw ">>, CalcType, <<"...">>]),
    Character = proplists:get_value(character, Props),
    case Character of
        Source ->
            log(debug, [self(), <<": Source (">>, Source, <<") is our character (">>, Character]),
            erlmud_attack:update_attack(Attack, source, Props);
        Target ->
            log(debug, [self(), <<": Target (">>, Target, <<") is our character (">>, Character]),
            erlmud_attack:update_attack(Attack, target, Props);
        _ ->
            log(debug, [<<"item attack ">>, CalcType, <<" attempt failed for ">>,
                        self(), <<" since character ">>,
                        proplists:get_value(character, Props),
                        <<" is not equal to ">>, Source, <<" or ">>, Target]),
            undefined
    end;
attempt({_, _, _Msg}) ->
    undefined.

%% attack with this weapon succeeded, kick off a new attack process
succeed({Props, Attack = #attack{target = Target,
                                 attack = undefined}}) ->
    Name = proplists:get_value(name, Props),
    Args = [_Id = undefined,
            _Props = [{owner, self()},
                      {target, Target},
                      {name, <<"attack_", Name/binary>>},
                      {handlers, [erlmud_handler_attack,
                                  erlmud_handler_set_child_property]}]],
    {ok, AttackPid} = supervisor:start_child(erlmud_object_sup, Args),
    log(debug, [<<"Attack ">>, AttackPid, <<" started, sending attempt and subscribing\n">>]),
    erlmud_object:attempt(Attack,
                          Attack#attack{attack = Attack},
                          _ShouldSub = true),
    [{attack, Attack} | Props];
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
