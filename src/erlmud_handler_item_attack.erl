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
-module(erlmud_handler_item_attack).
-behaviour(erlmud_handler).

%% This handler is specific to items and controls whether this item
%% process can participate in an attack or not. The character will
%% kick off a generic attack and then the generic attack handler attached
%% to this item process will further kick off a process-specific attack
%% for this process. This handler will listen to that specific attack
%% for it's process and determine if the properties of this item
%% allow for the item to attack. This prevents us from having logic in
%% the generic handler that is specific to items.
%% The generic handler can work out kicking off the hit roll and damage
%% since other handlers and other processes (e.g. attributes) will modify
%% the hit roll and damage with logic specific to this character, body part,
%% item, etc.

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

%% Attacking
attempt({#parents{character = Character},
         Props,
         {Character, attack, _Target}}) ->
    {succeed, true, Props};

attempt({#parents{character = Character},
         Props,
         {Character, attack, _Target, with, Self}})
  when Self == self() ->
    case is_interested(Props) of
        true ->
            {succeed, true, Props};
        false ->
            {{fail, <<"Item is not wielded or is not activated">>}, false, Props}
    end;

attempt({#parents{},
         Props,
         {allocate, _Required, 'of', _Type, to, Self}})
  when Self == self() ->
    {succeed, true, Props};

%% TODO handle counterattack and, if we're already attacking something,
%%      decide whether to switch targets.
%%      (maybe even kick of a 'switch_targets' attack)

%% Defending
%% I don't think we need to know when someone attacks our character,
%% we'll automatically get events for calc-hit and calc-damage

%% Defend
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, Hit, on, Character, with, AttackVector}}) ->
    case is_interested(Props) of
        true ->
            case proplists:get_value(defend_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Attacker, calc, Hit - Amount, on, Character, with, AttackVector}}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{character = Character},
         Props,
         {Attacker, damage, Damage, to, Character, with, AttackVector}}) ->
    case is_interested(Props) of
        true ->
            case proplists:get_value(defend_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Attacker, calc, Damage - Amount, on, Character, with, AttackVector}}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, attack, Target}}) when is_pid(Target) ->
    erlmud_object:attempt(self(), {Character, attack, Target, with, self()}),
    Props;

%% An attack by our character has been successfully instigated using this process:
%% we'll register for resources and implement the attack when we have them.
succeed({Props, {Character, attack, Target, with, _Self}}) ->
    reserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, Target}),
    _Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, true});

succeed({Props, {Character, stop_attack}}) ->
    unreserve(Character, Props),
    Props2 = lists:keystore(target, 1, Props, {target, undefined}),
    _Props3 = lists:keystore(is_attacking, 1, Props2, {is_attacking, false});

succeed({Props, {allocate, Amt, 'of', Type, to, Self}})
  when Self == self() ->
    Allocated = update_allocated(Amt, Type, Props),
    log(debug, [<<"Allocated = ">>, Allocated]),
    Required = proplists:get_value(resources, Props, []),
    log(debug, [<<"Required = ">>, Required]),
    RemainingAllocated =
        case has_resources(Allocated, Required) of
            true ->
                log(debug, [<<"Has resources">>]),
                attack(Props),
                deallocate(Allocated, Required);
            _ ->
                log(debug, [<<"Does not have resources">>]),
                Allocated
        end,
    _Props = lists:keystore(allocated_resources, 1, Props, {allocated_resources, RemainingAllocated});

succeed({Props, {Character, calc, Hit, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       Hit > 0 ->
    Damage = proplists:get_value(attack_damage_modifier, Props, 1),
    erlmud_object:attempt(self(), {Character, calc, Damage, to, Target, with, Self}),
    Props;

succeed({Props, {_Character, calc, _Miss, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    % TODO: say "you missed!"
    Props;

succeed({Props, {Character, calc, Damage, to, Target, with, Self}})
  when Self == self(),
       Damage > 0 ->
    erlmud_object:attempt(self(), {Character, does, Damage, to, Target, with, Self}),
    Props;

succeed({Props, {_Character, calc, _NoDamage, to, _Target, with, Self}})
  when Self == self() ->
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    Props;

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

attack(Props) ->
    Character = proplists:get_value(character, Props),
    Target = proplists:get_value(target, Props),
    Hit = proplists:get_value(attack_hit_modifier, Props, 1),
    erlmud_object:attempt(self(), {Character, calc, Hit, on, Target, with, self()}).

is_interested(Props) ->
    is_wielded(Props) andalso is_active(Props).

is_wielded(Props) ->
    BodyPart = proplists:get_value(body_part, Props),
    is_wielded(BodyPart, Props).

is_wielded({BodyPart, BodyPartType}, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    lists:member(BodyPartType, WieldingBodyParts);
is_wielded(_, _) ->
    false.

is_active(Props) ->
    true == proplists:get_value(is_active, Props, false).

unreserve(Character, Props) when is_list(Props) ->
    [unreserve(Character, Resource) || {Resource, _Amt} <- proplists:get_value(resources, Props, [])];
unreserve(Character, Resource) ->
    erlmud_object:attempt(self(), {Character, unreserve, Resource, for, self()}).

reserve(Character, Props) when is_list(Props) ->
    log(debug, [<<"Reserving">>]),
    [reserve(Character, Resource, Amount) || {Resource, Amount} <- proplists:get_value(resources, Props, [])].

reserve(Character, Resource, Amount) ->
    log(debug, [<<"Reserving">>, list_to_binary(integer_to_list(Amount)), <<"of">>, list_to_binary(atom_to_list(Resource))]),
    erlmud_object:attempt(self(), {Character, reserve, Amount, 'of', Resource, for, self()}).

update_allocated(New, Type, Props) ->
    Allocated = proplists:get_value(allocated_resources, Props, #{}),
    Curr = maps:get(Type, Allocated, 0),
    Allocated#{Type => Curr + New}.

deallocate(Allocated, Required) ->
    lists:foldl(fun subtract_required/2, Allocated, Required).

subtract_required({Type, Required}, Allocated) ->
    #{Type := Amt} = Allocated,
    Allocated#{Type := min(0, Amt - Required)}.

has_resources(Allocated, Required) ->
    {_, AllocApplied} = lists:foldl(fun apply_resource/2, {Allocated, []}, Required),
    case lists:filter(fun is_resource_lacking/1, AllocApplied) of
        [] ->
            true;
        _ ->
            false
    end.

apply_resource(_Resource = {Type, Required},
               {Allocated, Applied0}) ->
    AllocAmt = maps:get(Type, Allocated, 0),
    Applied1 = [{Type, Required - AllocAmt} | Applied0],
    {Allocated#{Type => 0}, Applied1}.

is_resource_lacking({_Type, Amount}) when Amount =< 0 ->
    false;
is_resource_lacking(_) ->
    true.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
