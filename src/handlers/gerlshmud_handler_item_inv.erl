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
-module(gerlshmud_handler_item_inv).
-behaviour(gerlshmud_handler).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% Track the current owner. When the 'add' succeeds the current owner can remove
%% it from its properties.
%%
%% The reason for "limited, to, item_body_parts" is that there are two conditions that have
%% to be met for an item to be added to a body part:
%% - the body part must have available space (e.g. an empty hand can hold a gun)
%% - the item must fit on that body part (e.g. an axe isn't going to be a hat)
%% This requires both the body part and the item to each contribute to the message
%% before we can check if they are met. We add two placeholder flags to the message:
%% - 'first_available_body_part' if we don't know which part it will be yet
%% - 'limited', 'to', 'item_body_parts' if we don't know what body part types are valid
%%   for the body part.
%% This clause fills in the 'item_body_parts' place-holder with the list of body
%% parts this item will fit on.
attempt({#parents{owner = Owner}, Props,
         {Self, move, from, Owner, to, Target, limited, to, item_body_parts}})
  when Self == self(),
       Owner /= Target ->
    BodyParts = proplists:get_value(body_parts, Props, []),
    NewMessage = {Self, move, from, Owner, to, Target, limited, to, BodyParts},
    Result = {resend, Owner, NewMessage},
    {Result, _Subscribe = false, Props};
attempt({#parents{owner = Owner}, Props,
         {Self, move, from, Owner, to, Target}})
  when Self == self(),
       Owner /= Target,
       is_pid(Target) ->
    {succeed, true, Props};
attempt({#parents{owner = Owner}, Props,
         {Self, move, from, Owner, to, Target, on, body_part, type, _BodyPartType}})
  when Self == self(),
       Owner /= Target,
       is_pid(Target) ->
    {succeed, true, Props};
attempt({#parents{}, Props,
         {Item, move, from, Self, to, Target}})
  when Self == self(),
       is_pid(Item),
       is_pid(Target) ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

%% Move to body part
succeed({Props, {Self, move, from, _OldOwner, to, NewOwner, on, body_part, type, BodyPartType}})
  when Self == self() ->
    %% If we wield something, it becomes active automatically
    IsWielded = is_wielded({NewOwner, BodyPartType}, Props),
    IsAutoActive = proplists:get_value(is_auto_active, Props, false),
    IsActive = proplists:get_value(is_active, Props, false) orelse
               (IsWielded andalso IsAutoActive),
    NewTopItemRef = make_ref(),
    Props2 = lists:foldl(fun apply_prop/2,
                        Props,
                        [{owner, NewOwner},
                         {body_part, {NewOwner, BodyPartType}},
                         {is_wielded, IsWielded},
                         {is_active, IsActive},
                         {top_item_ref, NewTopItemRef}]),
    set_child_properties(Props2),
    Props2;

%% Move to non-body part
%% New is_wielded, top_item and body_part properties will come from
%% the new owner, if applicable, in a separate event.
%% The new owner could be another parent item, a room, a character, etc.
%% The case of being added to a different item is the clause below this one.
%% TODO I think I need to store what an item was wielded by, then I can clear
%% it out if it still has that wielding body part: this could still break down
%% if the user transfers back and forth between two body parts: the first clear
%% might happen after the item has come back to the first body part. I might
%% need to add a ref as well. Except body parts are handled differently, so it
%% would have to be wielded by a body part, then not wielded, then wielded again.
succeed({Props, {Self, move, from, _OldOwner, to, NewOwner}})
  when Self == self() ->
    lists:keystore(owner, 1, Props, {owner, NewOwner});

%% gaining an item
succeed({Props, {Item, move, from, Source, to, Self}}) when Self == self() ->
    log([{?EVENT, move},
         {object, Self},
         {props, Props},
         {item, Item},
         {?SOURCE, Source},
         {?TARGET, Self},
         {result, succeed}]),
    set_child_properties(Item, Props),
    [{item, Item} | Props];

%% Losing an item
succeed({Props, {Item, move, from, Self, to, Target}}) when Self == self() ->
    clear_child_top_item(Props, Item, Target);

%% Losing an item to a body part
succeed({Props, {Item, move, from, Self, to, Target, on, body_part, type, _BodyPartType}}) when Self == self() ->
    clear_child_top_item(Props, Item, Target);

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

set_child_properties(Props) ->
    set_child_properties(self(), Props).

set_child_properties(Child, Props) ->
    TopItem =
        case proplists:get_value(top_item, Props) of
            TopItem_ = #top_item{} ->
                TopItem_;
            undefined ->
                #top_item{item = self(),
                          is_active = gerlshmud_object:value(is_active, Props, boolean),
                          is_wielded = is_wielded(Props),
                          ref = proplists:get_value(top_item_ref, Props)}
        end,
    ChildProps = [{top_item, TopItem}],
    gerlshmud_object:attempt(Child, {self(), set_child_properties, ChildProps}).

clear_child_top_item(Props, Item, Target) ->
    log([{?EVENT, give},
         {object, self()},
         {props, Props},
         {item, Item},
         {?SOURCE, self()},
         {?TARGET, Target},
         {result, succeed}]),
    TopItem = top_item(Props),
    Message = {Target, clear_child_property, top_item, 'if', TopItem},
    gerlshmud_object:attempt(Item, Message),
    lists:keydelete(Item, 2, Props).

top_item(Props) ->
    TopItemRef = proplists:get_value(top_item_ref, Props),
    % Only clear the top item if the ref still matches, otherwise
    % a new top item has been set (even by this process if the item
    % leaves and comes back and the events get out of order)
    DefaultTopItem = #top_item{item = self(), ref = TopItemRef},
    proplists:get_value(top_item, Props, DefaultTopItem).

is_wielded(Props) ->
    BodyPart = proplists:get_value(body_part, Props),
    %% maybe our top-item is wielded
    IsWielded = proplists:get_value(is_wielded, Props, false),
    %% ... or maybe we're a top-item and wielded on a body part with
    %% a type that matches our 'wielding_body_parts', or types of body
    %% parts upon which, when equipped, we are considered wielded.
    IsWielded orelse is_wielded(BodyPart, Props).

is_wielded({BodyPart, BodyPartType}, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    lists:member(BodyPartType, WieldingBodyParts);
is_wielded(_, _) ->
    false.

apply_prop({K, V}, Props) ->
    lists:keystore(K, 1, Props, {K, V}).

log(Props) ->
    gerlshmud_event_log:log(debug, [{module, ?MODULE} | Props]).
