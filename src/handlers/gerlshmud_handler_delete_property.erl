%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_delete_property).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-include("include/gerlshmud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{},
         Props,
         {delete, Pid}}) ->
    Log = [{?SOURCE, Pid},
           {?EVENT, delete}],
    Props2 = lists:keydelete(Pid, 2, Props),
    {succeed, false, Props2, Log};
attempt(_) ->
    undefined.

succeed({Props, _Msg}) ->
    Props.

fail({Props, _, _}) ->
    Props.
