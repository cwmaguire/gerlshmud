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
-module(erlmud_handler_char_inv).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% Injects the room, which might indicate this should be in ...room_inject_self,
%% except the character has a 'room' property, which is faster.
%% Also, characters can only be owned by rooms. This wouldn't work
%% for a item owned by a body part because an item might be owned by a
%% character, room or other item (e.g. container).
attempt({_Owner, Props, {Self, Action, Item}})
  when Self == self() andalso
       is_pid(Item) andalso
       Action == get; Action == drop ->
    case Action == get orelse erlmud_object:has_pid(Props, Item) of
        true ->
            Room = proplists:get_value(owner, Props),
            {Source, Target} = case Action of
                drop ->
                    {Self, Room};
                get ->
                    {Room, Self}
            end,
            {{resend, Self, {move, Item, from, Source, to, Target}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt({_Owner, Props, {move, Item, from, Self, to, Room}})
  when Self == self() andalso
       is_pid(Item),
       is_pid(Room) ->
    {succeed, true, Props};
attempt({_Owner, Props, {move, Item, from, Self, to, BodyPart, _ItemBodyParts}})
  when Self == self() andalso
       is_pid(Item),
       is_pid(BodyPart) ->
    {succeed, true, Props};
attempt({_Owner, Props, {move, Item, from, _Room, to, Self}})
  when Self == self() andalso
       is_pid(Item) ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {move, Item, from, Source, to, Self}}) when Self == self() ->
    log(debug, [<<"Getting ">>, Item, <<" from ">>, Source, <<"\n">>]),
    erlmud_object:attempt(Item, {set_character, self(), self()}),
    [{item, Item} | Props];
succeed({Props, {move, Item, from, Self, to, Target}}) when Self == self() ->
    log(debug, [<<"Giving ">>, Item, <<" to ">>, Target, <<"\n\tProps: ">>, Props, <<"\n">>]),
    lists:keydelete(Item, 2, Props);
succeed({Props, {move, Item, from, Self, to, Target, _ItemBodyParts}}) when Self == self() ->
    log(debug, [<<"Giving ">>, Item, <<" to ">>, Target, <<"\n\tProps: ">>, Props, <<"\n">>]),
    lists:keydelete(Item, 2, Props);
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
