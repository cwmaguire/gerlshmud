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

-include("include/erlmud.hrl").

%% Attacking: hit and damage
attempt({#parents{top_item = TopItem},
         Props,
         {Character, calc, Hit, on, Target, with, TopItem}}) ->
    case is_interested(Props) of
        true ->
            case proplists:get_value(attack_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Character, calc, Hit + Amount, on, Target, with, TopItem}}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{top_item = TopItem},
         Props,
         {Character, damage, Damage, to, Target, with, TopItem}}) ->

    case is_interested(Props) of
        true ->
            case proplists:get_value(attack_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Character, calc, Damage + Amount, on, Target, with, TopItem}}
            end;
        _ ->
            {succeed, false, Props}
    end;

%% Defending: hit and damage
%% I'm going to have to have top items broadcast wielded
%% and active when their states change.
attempt({#parents{top_item = #top_item{item = TopItem}},
         Props,
         {Attacker, calc, Hit, on, Character, with, AttackVector}}) ->
    case is_interested(Props) of
        true ->
            case proplists:get_value(attack_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Character, calc, Hit + Amount, on, Target, with, TopItem}}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{top_item = TopItem},
         Props,
         {Character, damage, Damage, to, Target, with, TopItem}}) ->

    case is_interested(Props) of
        true ->
            case proplists:get_value(attack_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed, {Character, calc, Damage + Amount, on, Target, with, TopItem}}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, attack, Target}}) when is_pid(Target) ->
    erlmud_object:attempt(self(), {Character, attack, Target, with, self()}),
    Props;
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_interested(Props) ->
    IsActive = true =:= proplists:get_value(active, Props),
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
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
