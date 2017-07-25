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
-module(erlmud_handler_body_part_inject_self).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, Owner, to, BodyPartName}})
  %when is_pid(Item) andalso
       %is_binary(BodyPartName) ->
  when is_binary(BodyPartName) ->
    case is_match(Props, BodyPartName) of
        true ->
            NewMessage = {Item, move, from, Owner, to, self()},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, BodyPartName, to, Owner}})
  when is_pid(Item) andalso
       is_binary(BodyPartName) ->
    case is_match(Props, BodyPartName) of
        true ->
            NewMessage = {Item, move, from, self(), to, Owner},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, Owner, to, first_available_body_part}})
  when is_pid(Item) ->
    NewMessage = {Item, move, from, Owner, to, first_available_body_part, limited, to, item_body_parts},
    Result = {resend, Owner, NewMessage},
    {Result, _Subscribe = true, Props};
%
% This was already mostly done in erlmud_handler_body_part_inv
%attempt({#parents{owner = Owner},
         %Props,
         %{Item, move, from, Owner, to, first_available_body_part, limited, to, ItemBodyParts}})
  %when is_pid(Item),
       %is_list(ItemBodyParts) ->
    %% Not sure if I should just move this clause over to erlmud_handler_body_part_inv
    %case erlmud_handler_body_part_inv:can_add(Props, ItemBodyParts) of
        %true ->
            %BodyPartType = proplists:get_value(body_part, Props, undefined),
            %NewMessage = {Item, move, from, Owner, to, self(), on, body_part, type, BodyPartType},
            %Result = {resend, Owner, NewMessage},
            %{Result, _Subscribe = true, Props};
        %_ ->
            %{succeed, _Subscribe = false, Props}
    %end;
attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, current_body_part, to, Owner}}) ->
    %% We can't have the room inject itself because a room might
    %% be owned by  a room, character, another item, etc.
    case {item, Item} == lists:keyfind(Item, 2, Props) of
        true ->
            NewMessage = {Item, move, from, self(), to, Owner},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_match(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, <<>>), Name, [{capture, none}]).

%log(Level, IoData) ->
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).

