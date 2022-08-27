%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_subitem_attack).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

%% This handler is specific to sub-items and controls whether this sub-item
%% process can participate in an attack or not. The character will
%% kick off a generic attack and then the generic attack handler attached
%% to this item process will further kick off a process-specific attack
%% for this process. This handler will listen to that specific attack
%% for it's process and determine if the properties of this sub-item
%% allow for the sub-item to attack. This prevents us from having logic in
%% the generic handler that is specific to items.
%% The generic handler can work out kicking off the hit roll and damage
%% since other handlers and other processes (e.g. attributes) will modify
%% the hit roll and damage with logic specific to this character, body part,
%% item, etc.
%%
%% This is very similar to the item attack handler except this handler
%% checks if this sub-item belongs to an attacking or defending parent
%% item.

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

%% Attacking: hit and damage
attempt({#parents{top_item = TopItem},
         Props,
         {Character, calc, Hit, on, Target, with, TopItem}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, calc_hit},
           {hit, Hit},
           {?TARGET, Target},
           {vector, TopItem}],
    case is_interested(Props) of
        true ->
            case proplists:get_value(attack_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props, Log};
                Amount ->
                    {succeed, {Character, calc, Hit + Amount, on, Target, with, TopItem}, Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{top_item = TopItem},
         Props,
         {Character, damage, Damage, to, Target, with, TopItem}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, damage},
           {damage, Damage},
           {?TARGET, Target},
           {vector, TopItem}],
    case is_interested(Props) of
        true ->
            case proplists:get_value(attack_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props, Log};
                Amount ->
                    {succeed, {Character, calc, Damage + Amount, on, Target, with, TopItem}, Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;

%% Defending: hit and damage
%% I'm going to have to have top items broadcast wielded
%% and active when their states change.
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, Hit, on, Character, with, AttackVector}}) ->
    Log = [{?SOURCE, Attacker},
           {?EVENT, calc_hit},
           {hit, Hit},
           {?TARGET, Character},
           {vector, AttackVector}],
    case is_interested(Props) of
        true ->
            case proplists:get_value(defence_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props, Log};
                Amount ->
                    {succeed, {Character, calc, Hit + Amount, on, Character, with, AttackVector}, Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{character = Character},
         Props,
         {Character, damage, Damage, to, Target, with, AttackVector}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, damage},
           {damage, Damage},
           {?TARGET, Target},
           {vector, AttackVector}],
    case is_interested(Props) of
        true ->
            case proplists:get_value(defence_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props, Log};
                Amount ->
                    ModifiedMessage = {Character, calc, Damage + Amount, on, Target, with, AttackVector},
                    {succeed, ModifiedMessage, Log}
            end;
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, attack, Target}}) when is_pid(Target) ->
    Log = [{?SOURCE, Character},
           {?EVENT, attack},
           {?TARGET, Target}],
    gerlshmud_object:attempt(self(), {Character, attack, Target, with, self()}),
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_interested(Props) ->
    IsActive = true =:= proplists:get_value(is_active, Props),
    IsActive andalso is_top_item_interested(Props).

is_top_item_interested(Props) ->
    case proplists:get_value(top_item, Props) of
        #top_item{is_wielded = true,
                  is_active = true} ->
            true;
        _ ->
            false
    end.

%log(Level, IoData) ->
    %gerlshmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
