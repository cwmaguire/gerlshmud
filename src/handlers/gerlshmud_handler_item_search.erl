%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_item_search).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{owner = Char},
         Props,
         {Player, search, Char, named, _Name, with, body_parts, _BodyParts}}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Char}],
    {succeed, true, Props, Log};
attempt({#parents{owner = Owner},
         Props,
         {Player, search, Char, named, _Name, with, body_parts, BodyParts}}) ->
    Log = [{?EVENT, search},
           {?SOURCE, Player},
           {?TARGET, Char}],
    ShouldSubscribe = lists:member(Owner, BodyParts),
    {succeed, ShouldSubscribe, Props, Log};
attempt(_) ->
    undefined.

succeed({Props,
         {Player, search, _Char, named, Name, with, body_parts, _BodyParts}}) ->
    Context = <<Name/binary, " has ">>,
    send_description(Player, Props, Context),
    Props;
succeed({Props, _Msg}) ->
    Props.

fail({Props, _Result, _Msg}) ->
    Log = [{?EVENT, search},
           {?TARGET, self()}],
    {Props, Log}.

send_description(Source, Props, Context) ->
    Description = gerlshmud_util:describe(item, Props),
    SendMsg = {send, Source, [<<Context/binary>>, Description]},
    gerlshmud_object:attempt(Source, SendMsg).
