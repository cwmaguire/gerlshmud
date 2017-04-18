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
attempt({#parents{character = Character,
                  body_part = BodyPart},
         Props,
         {Character, attack, _Target, with, Self}})
  when Self == self() ->
    case is_wielded(BodyPart, Props) andalso active(Props) of
        true ->
            {true, true, Props};
        false ->
            {false, "Item is not wielded or is not activated"}
    end;

%% TODO handle counterattack and, if we're already attacking something,
%%      decide whether to switch targets.
%%      (maybe even kick of a 'switch_targets' attack)

%% Defending
%% I don't think we need to know when someone attacks our character,
%% we'll automatically get events for calc-hit and calc-damage

%% Attack
attempt({#parents{character = Character},
         Props,
         {Character, calc, Hit, on, Target, with, Self}})
  when Self == self() ->
    case is_interested(Props) of
        true ->
            case proplists:get_value(attack_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Character, calc, Hit + Amount, on, Target, with, Self}}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{character = Character},
         Props,
         {Character, damage, Damage, to, Target, with, Self}})
  when Self == self() ->
    case is_interested(Props) of
        true ->
            case proplists:get_value(attack_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Character, calc, Damage + Amount, on, Target, with, Self}}
            end;
        _ ->
            {succeed, false, Props}
    end;

%% Defend
attempt({#parents{character = Character},
         Props,
         {Character, calc, Hit, on, Target, with, Self}})
  when Self == self() ->
    case is_interested(Props) of
        true ->
            case proplists:get_value(defend_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Character, calc, Hit - Amount, on, Target, with, Self}}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{character = Character},
         Props,
         {Character, damage, Damage, to, Target, with, Self}})
  when Self == self() ->
    case is_interested(Props) of
        true ->
            case proplists:get_value(defend_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Character, calc, Damage - Amount, on, Target, with, Self}}
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
    reserve(Character, proplists:get_value(resources, Props, [])),
    lists:keystore(target, 1, Props, {target, Target});

succeed({Props, {Character, stop_attack}}) ->
    unreserve(Character, Props),
    [{is_attacking, false} | Props];

succeed({Props, {Character, calc, Hit, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       Hit > 0 ->
    erlmud_object:attempt(self(), {Character, calc, _InitialDamage = 0, to, Target, with, Self}),
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

is_interested(Props) ->
    true =:= is_wielded(Props) andalso
    true =:= proplists:get_value(active, Props).

is_wielded(Props) ->
    BodyPart = proplists:get_value(body_part, Props),
    is_wielded(BodyPart, Props).

is_wielded(BodyPart, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    case proplists:get_value(body_part, Props) of
        {_, BodyPartType} ->
            lists:member(BodyPartType, WieldingBodyParts);
        _ ->
            false
    end.

active(Props) ->
    proplists:get_value(active, Props, false).

unreserve(Owner, Props) ->
    reserve_op(unreserve, Owner, Props).

reserve(Owner, Props) ->
    reserve_op(reserve, Owner, Props).

reserve_op(Op, Character, Props) when is_list(Props) ->
    [reserve_op(Op, Character, R) || R <- proplists:get_value(resources, Props, [])];

reserve_op(Op, Character, Resource) ->
    erlmud_object:attempt(self(), {Character, Op, Resource, for, self()}).

%log(Level, IoData) ->
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
