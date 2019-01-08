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
-module(erlmud_handler_attribute_attack).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

%% @doc modify an attack with this attribute's modifiers
%%
%% This is to model things like strength, charisma, dexterity,
%% race, etc.
%%
%% This attribute can be for a character, a body part or an item.
%%

%% Attack
attempt({#parents{character = Character,
                  top_item = TopItem = #top_item{item = Item}},
         Props,
         {Character, calc, Hit, on, Target, with, Item}}) ->
    case is_interested(TopItem, Props) of
        true ->
            case proplists:get_value(attack_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed,
                     {Character, calc, Hit + Amount, on, Target, with, Item},
                     false,
                     Props}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{character = Character,
                  top_item = TopItem = #top_item{item = Item}},
         Props,
         {Character, damage, Damage, to, Target, with, Item}}) ->
    case is_interested(TopItem, Props) of
        true ->
            case proplists:get_value(attack_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed,
                     {Character, calc, Damage + Amount, on, Target, with, Item},
                     false,
                     Props}
            end;
        _ ->
            {succeed, false, Props}
    end;

%% Defend with item
attempt({#parents{character = Character,
                  top_item = TopItem = #top_item{item = Item}},
         Props,
         {Attacker, calc, Hit, on, Character, with, Item}}) ->
    case is_interested(TopItem, Props) of
        true ->
            case proplists:get_value(defence_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed,
                     {Attacker, calc, Hit - Amount, on, Character, with, Item},
                     false,
                     Props}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{character = Character,
                  top_item = TopItem = #top_item{item = Item}},
         Props,
         {Target, damage, Damage, to, Character, with, Item}}) ->
    case is_interested(TopItem, Props) of
        true ->
            case proplists:get_value(defence_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed,
                     {Target, calc, Damage - Amount, on, Character, with, Item},
                     false,
                     Props}
            end;
        _ ->
            {succeed, false, Props}
    end;

%% Defend without item
attempt({#parents{character = Character},
         Props,
         {Attacker, calc, Hit, on, Character, with, Item}}) ->
    case is_interested(not_an_item, Props) of
        true ->
            case proplists:get_value(defence_hit_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed,
                     {Attacker, calc, Hit - Amount, on, Character, with, Item},
                     false,
                     Props}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({#parents{character = Character},
         Props,
         {Target, damage, Damage, to, Character, with, Item}}) ->
    case is_interested(not_an_item, Props) of
        true ->
            case proplists:get_value(defence_damage_modifier, Props) of
                undefined ->
                    {succeed, false, Props};
                Amount ->
                    {succeed,
                     {Target, calc, Damage - Amount, on, Character, with, Item},
                     false,
                     Props}
            end;
        _ ->
            {succeed, false, Props}
    end;
attempt({_, _, _Msg}) ->
    undefined.

is_interested(#top_item{is_wielded = true,
                        is_active = true},
              _Props) ->
    true;
is_interested(#top_item{}, Props) ->
    proplists:get_value(must_be_wielded, Props, false);
is_interested(_, _) ->
    %% Everything that isn't bound to an item is always active for now
    %% e.g. character attribute
    true.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
