%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_room_look).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({_Owner, Props, {_Source, look, Self}}) when Self == self() ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {Player, look, Self}}) when Self == self() ->
    Log = [{?SOURCE, Player},
           {?EVENT, look},
           {?TARGET, Self}],
    describe(Player, Props),
    Name = proplists:get_value(name, Props, <<"room with no name">>),
    RoomContext = <<Name/binary, " -> ">>,
    %% Resend as Player looking at this Room with Context
    %% which is a key to objects in this room to describe themselves
    NewMessage = {Player, describe, self(), with, RoomContext},
    gerlshmud_object:attempt(Player, NewMessage),
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

describe(Source, Props) ->
    Description = description(Props),
    gerlshmud_object:attempt(Source, {send, Source, Description}).

description(Props) when is_list(Props) ->
    DescTemplate = gerlshmud_config:desc_template(room),
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
    gerlshmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
