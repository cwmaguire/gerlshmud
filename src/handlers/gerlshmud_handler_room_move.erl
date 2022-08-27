%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_room_move).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({_Owner, Props, {Char, move, from, Source, to, Target, via, Exit}})
  when Source == self(); Target == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    {succeed, true, Props, Log};
attempt({_Owner, Props, {Char, enter_world, in, Self, with, Conn}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, enter_world},
           {room, Self},
           {conn, Conn}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Char, move, from, Self, to, Target, via, Exit}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Self},
           {to, Target},
           {exit, Exit}],
    Props2 = lists:keydelete(Char, 2, Props),
    {Props2, Log};
succeed({Props, {Char, move, from, Source, to, Self, via, Exit}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Self},
           {exit, Exit}],
    gerlshmud_object:attempt(Self, {Char, look, Self}),
    Props2 = [{character, Char} | Props],
    {Props2, Log};
succeed({Props, {Char, enter_world, in, Self, with, Conn}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, enter_world},
           {room, Self},
           {conn, Conn}],
    gerlshmud_object:attempt(Self, {Char, look, Self}),
    Props2 = [{character, Char} | Props],
    {Props2, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _Reason, {Char, move, from, Self, to, Target}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Self},
           {to, Target}],
    {Props, Log};
fail({Props, _Reason, {Char, move, from, Source, to, Target, via, Exit}}) when Source == self(); Target == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    {Props, Log};
fail({Props, _, _}) ->
    Props.
