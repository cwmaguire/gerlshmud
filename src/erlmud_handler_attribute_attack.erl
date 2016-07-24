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
%% This attribute can be for a character, a body part or an item.
%%
%% TODO: make attributes either passive or active (ie. wielded/worn)
%% so that we don't need to figure out in the attack code which it
%% is
attempt({Owner, Props, Attack = #attack{calc_type = CalcType}})
  when (CalcType == hit orelse
        CalcType == damage orelse
        CalcType == wait) ->
    %% Am I on an item?  then I'll have top_item
    %% Am I active?  then I'll have is_active = true
    case proplists:get_value(is_active
    case source_or_target(Owner, Attack, Props) of
        source ->
            erlmud_attack:update_attack(Attack, source, Props);
        dest ->
            erlmud_attack:update_attack(Attack, target, Props);
        undefined ->
            {succeed, false, Props}
    end;
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

source_or_target(Owner, Attack, Props) ->
    Character = proplists:get_value(character, Props),
    BodyPart = proplists:get_value(body_part, Props),
    TopItem = proplists:get_value(top_item, Props),
    source_or_target(Owner, Attack, Character, Item, BodyPart).

%% Character attribute: owner is character - can be source
%% Body Part attribute: owner is body_part - can be weapon
%% Item attribute: 'character' is character - can be source
%% Sub-item attribute: 'character' is character - can be source,
%%                     'top_item' is item - can be weapon
source_or_target(#attack{source = Owner}, Owner, _, _, _) ->
    source;
source_or_target(#attack{source = Character}, _, Character, _, _) ->
    source;
% Weapons, as Items, already have the character set on them when
% they are moved to the character. So if the weapon was used in an
% attacked the Character will back the source
%source_or_target(#attack{weapon = Item}, _, _, Item, _) ->
    %source;
source_or_target(#attack{weapon = BodyPart}, _, _, _, BodyPart) ->
    source;
source_or_target(#attack{target = Owner}, Owner, _, _, _) ->
    target;
source_or_target(#attack{target = Character}, _, Character, _, _) ->
    target;
%% There is no target weapon; the weapon is what the attack initiator
%% is using to attack. The target can defend with anything that is
%% wielded
%source_or_target(#attack{weapon = Item}, _, _, Item, _) ->
    %target;
%source_or_target(#attack{weapon = BodyPart}, _, _, _, BodyPart) ->
    %target;
source_or_target(_, _, _, _, _) ->
    undefined.

%log(Level, IoData) ->
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
