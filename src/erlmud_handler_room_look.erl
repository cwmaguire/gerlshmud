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
-module(erlmud_handler_room_look).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Owner, Props, {_Source, look, Self}}) when Self == self() ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {Player, look, Self}}) when Self == self() ->
    log([<<"Process ">>, Player, <<" looked at me">>]),
    describe(Player, Props),
    Name = proplists:get_value(name, Props, <<"room with no name">>),
    RoomContext = <<Name/binary, " -> ">>,
    %% Resend as Player looking at this Room with Context
    %% which is a key to objects in this room to describe themselves
    NewMessage = {Player, describe, self(), RoomContext},
    erlmud_object:attempt(Player, NewMessage),
    Props;
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

describe(Source, Props) ->
    Description = description(Props),
    erlmud_object:attempt(Source, {send, Source, Description}).

description(Props) when is_list(Props) ->
    DescTemplate = application:get_env(erlmud, room_desc_template, []),
    log([<<"description template: ">>, DescTemplate]),
    Description = [[description_part(Props, Part)] || Part <- DescTemplate],
    log([<<"Description: ">>, Description]),
    Description.

description_part(_, RawText) when is_binary(RawText) ->
    log([<<"description_part with unknown Props and RawText: ">>, RawText]),
    RawText;
description_part(Props, DescProp) ->
    log([<<"description_part with Props: ">>, Props, <<", DescProp: ">>, DescProp]),
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
