%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_conn_enter_world).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Owner, enter_world, in, Room, with, Self}}) when Self == self(), is_pid(Room) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, enter_world},
           {room, Room},
           {conn, Self}],
    {succeed, _Subscribe = true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Player, enter_world, in, Room, with, Conn}}) ->
    Log = [{?EVENT, enter_world},
           {?SOURCE, self()},
           {conn, Conn},
           {room, Room}],
    {[{owner, Player} | lists:keydelete(owner, 1, Props)], Log};
succeed({Props, _Other}) ->
    Props.

%% TODO test that failing to enter the world disconnects the player
fail({Props, Reason, {Player, enter_world}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, enter_world}],
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    {Props, Log};
fail({Props, _Reason, _Message}) ->
    Props.
