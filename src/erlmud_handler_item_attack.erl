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

attempt({#parents{character = Character,
                  body_part = BodyPart},
         Props,
         {Character, attack, _Target, with, Self}})
  when Self == self() ->
    case wielded(BodyPart, Props) andalso active(Props) of
        true ->
            {true, true, Props};
        false ->
            {false, "Item is not wielded"}
    end;

%% If our top-item is attacking then we want to subscribe to the
%% hit- and damage-calc messages so we can participate
attempt({#parents{top_item = TopItem},
         Props,
         {Character, calc, Hit, on, Target, with, TopItem}}) ->

    case is_interested() of
        true ->
            case proplists:get_value(attack_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    %% In order to mix and match +/- with * I could have four values:
                    %% Minus integer,
                    %% Plus integer,
                    %% Minus multiplier,
                    %% Plus multiplier.
                    %%
                    %% Once the attempt is finished I could do:
                    %% (Minus integer * minus multiplier) + (Plus integer * plus multiplier)
                    %%
                    %% e.g. if a sword does +2 to hit and a ring does * 1.1 to hit, then
                    %% I'd have to add up all the integers, multiple all the scaling, then
                    %% apply the scaling to the integers.
                    %%
                    %% e.g. A sword does +2 to hit, a ring does * 1.1 to hit, attacker dexteriy
                    %% does +1 to hit, attacker spell does * 2 against the enemy race, the enemy
                    %% armor does -3 to hit and the enemy dexterity does * 1.1 to hit. An enemy
                    %% priest does a -1 to hit buff and the weather does * 0.5 to hit.
                    %%
                    %% The enemy dexterity shouldn't apply to the _positive_ (i.e. successful)
                    %% numbers, it should multiple the negative (i.e. fail) numbers:
                    %%
                    %% Positive: (+2 + 1) * 1.1 * 2 * 0.5 = 3.3
                    %% Negative: (-1) * 1.1 = -1.1
                    %% Total: 3.3 - 1.1 = 2.2
                    %%
                    %% In this way I can have multiplier effects and additive/subtractive effects.
                    %% The base value is 1 so I could even have _only_ multiplier effects.
                    %% However, there might have to be a base negative amount as well in case I
                    %% only had negative multipliers.
                    %% That works out well because +1 + -1 = 0, ... i.e. if nothing affects the
                    %% attack it will fail with 0.
                    %%
                    %% So, I'd probably want weapons to have a base damage, and then have skills,
                    %% spells, attributes, environment, etc. potentially do multipliers.
                    %%
                    %% At that point I might have a Hit map instead of just a hit number:
                    %% #{pos => 1, neg => -1, pos_multiplier => 1, neg_multiplier => 1}
                    %%
                    %% e.g.
                    %% {succeed, {C, calc, neg_mult(Hit, NegMult), on, T, with, W}}
                    %%
                    %% where neg_mult/2 is a function that multiplies the negative multiplier
                    %% by it's amount. Since the negative multiplier starts out at one that works
                    %% out nicely.
                    {succeed, {Character, calc, Hit + Amount, on, Target, with, TopItem}}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{top_item = TopItem},
         Props,
         {Character, damage, Damage, to, Target, with, TopItem}}) ->

    case is_interested() of
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

is_interested() ->
    %% TODO fill this in. What might cause an item or sub-item not to be
    %% interested? ... not activated?
    ok.


wielded(BodyPart, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    case proplists:get_value(body_part, Props) of
        {_, BodyPartType} ->
            lists:member(BodyPartType, WieldingBodyParts);
        _ ->
            false
    end.

active(Props) ->
    proplists:get_value(active, Props, false).

%log(Level, IoData) ->
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
