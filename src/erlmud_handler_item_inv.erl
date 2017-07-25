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
-module(erlmud_handler_item_inv).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% Track the current owner. When the 'add' succeeds the current owner can remove
%% it from its properties.
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
    Props2 = lists:foldl(fun apply_prop/2,
                        Props,
                        [{owner, NewOwner},
                         {body_part, {NewOwner, BodyPartType}},
                         {is_wielded, IsWielded},
                         {is_active, IsActive}]),
    set_child_properties(Props2),
    Props2;

%% Move to non-body part
%% New is_wielded, top_item and body_part properties will come from
%% the new owner, if applicable, in a separate event.
succeed({Props, {Self, move, from, _OldOwner, to, NewOwner}})
  when Self == self() ->
    lists:keystore(owner, 1, Props, {owner, NewOwner});

%% gaining an item
succeed({Props, {Item, move, from, Source, to, Self}}) when Self == self() ->
    log(debug, [<<"Getting ">>, Item, <<" from ">>, Source, <<"\n">>]),
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
    TopItem = #top_item{item = self(),
                        is_active = erlmud_object:value(is_active, Props, boolean),
                        is_wielded = is_wielded(Props)},
    ChildProps = [{body_part, body_part(Props)},
                  {top_item, TopItem},
                  {is_active, erlmud_object:value(is_active, Props, boolean)},
                  {is_wielded, is_wielded(Props)}],
    erlmud_object:attempt(Child, {self(), set_child_properties, ChildProps}).

top_item(Props) ->
    DefaultTopItem = #top_item{item = self()},
    proplists:get_value(top_item, Props, DefaultTopItem).

body_part(Props) ->
    proplists:get_value(body_part, Props).

clear_child_top_item(Props, Item, Target) ->
    log(debug, [<<"Giving ">>, Item, <<" to ">>, Target, <<"\n\tProps: ">>, Props, <<"\n">>]),
    erlmud_object:attempt(Item, {Target, clear_child_property, top_item, 'if', top_item(Props)}),
    lists:keydelete(Item, 2, Props).

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

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
