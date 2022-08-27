%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_item_cleanup).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{},
         Props,
         {Character, cleanup,  body_parts, _BodyParts, in, Room}}) ->
    Log = [{?TARGET, Character},
           {?EVENT, cleanup},
           {room, Room}],
    {succeed, true, Props, Log};
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, cleanup, body_parts, BodyParts, in, Room}}) ->
    Log = [{?SOURCE, Character},
           {?TARGET, Room},
           {?EVENT, cleanup}],
    Owner = proplists:get_value(owner, Props),
    case lists:member(Owner, [Character | BodyParts]) of
        true -> gerlshmud_object:attempt(self(),
                                         {self(), move, from, Owner, to, Room},
                                         _ShouldSubscribe = false);
        _ ->
            ok
    end,
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
