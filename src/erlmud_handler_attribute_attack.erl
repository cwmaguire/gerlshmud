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
    case is_interested(activity_props(Props)) of
        true ->
            case source_or_target(Owner, Attack, Props) of
                source ->
                    erlmud_attack:update_attack(Attack, source, Props);
                dest ->
                    erlmud_attack:update_attack(Attack, target, Props);
                undefined ->
                    {succeed, false, Props}
            end;
        false ->
            {succeed, false, Props}
    end;
attempt({_, _, _Msg}) ->
    undefined.

is_interested({item, BodyPart, _}) when BodyPart /= undefined->
    true;
is_interested({item, _NoBodyPart, _MustBeInUse = false}) ->
    true;
is_interested({item, _NoBodyPart, _MustBeInUse}) ->
    false;
is_interested({_NotItem, _DoesntMatter, _DoesntMatter}) ->
    true.

activity_props(Props) ->
    activity_props(proplists:get_value(top_item, Props), Props).

%% When an item is in use it will be worn on, or wielded by, a body
%% part. That is, it will have a body part property, similar to
%% 'top_item', except that body parts don't have sub-parts.
activity_props(item, Props) ->
    activity_props(item, proplists:get_value(body_part, Props), Props);
activity_props(_, _) ->
    {not_item, undefined, undefined}.

activity_props(item, BodyPart = undefined, _Props) ->
    {item, BodyPart, undefined};
activity_props(item, BodyPart, Props) ->
    {item, BodyPart, proplists:get_value(must_be_in_use, Props, false)}.


succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

source_or_target(Owner, Attack, Props) when is_list(Props) ->
    Character = proplists:get_value(character, Props),
    source_or_target(Attack, Owner, Character);
source_or_target(#attack{source = Owner}, Owner, _) ->
    source;
source_or_target(#attack{source = Character}, _, Character) ->
    source;
source_or_target(#attack{weapon = BodyPart}, Owner, _) when BodyPart == Owner ->
    source;
source_or_target(#attack{target = Owner}, Owner, _) ->
    target;
source_or_target(#attack{target = Character}, _, Character) ->
    target;
source_or_target(_, _, _) ->
    undefined.

%log(Level, IoData) ->
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
