%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_char_enter_world).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({_Parents,
         Props,
         {Self, enter_world, in, room, with, Conn}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, enter_world},
           {conn, Conn}],
    case proplists:get_value(owner, Props) of
        undefined ->
            Log2 = [{?TARGET, undefined} | Log],
            {succeed, false, Props, Log2};
        Room ->
            Log2 = [{?TARGET, Room} | Log],
            NewMessage = {Self, enter_world, in, Room, with, Conn},
            {{resend, Self, NewMessage}, true, Props, Log2}
    end;
attempt({_Parents,
         Props,
         {Self, enter_world, in, Room, with, _Conn}}) when Self == self(), is_pid(Room) ->
    Log = [{?SOURCE, Self},
           {?EVENT, enter_world},
           {?TARGET, Room}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Player, enter_world, in, Room, with, Conn}}) ->
    Log = [{?EVENT, char_enter_world},
           {?SOURCE, Player},
           {?TARGET, Room},
           {conn, Conn}],
    Props2 = lists:foldl(fun keyreplace/2, Props, [{conn, Conn}]),
    {Props2, Log};
succeed({Props, _Other}) ->
    Props.

fail({Props, Reason, {Player, enter_world}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, enter_world}],
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    {Props, Log};
fail({Props, _Reason, _Message}) ->
    Props.

keyreplace(NewKV = {Key, _}, Props) ->
    [NewKV | lists:keydelete(Key, 1, Props)].
