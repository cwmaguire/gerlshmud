%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_search).

-behaviour(gerlshmud_object).

%% object behaviour
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

-define(PROPS, []).

id(_Props, Owner, Pid) ->
    "search_for" ++ Owner ++ "_" ++ Pid.

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(_Owner, Props, {search, Src, _Hierarchy}) ->
    NewMessage = {{resend, Src, _NewMessage = dunno_yet}, _ShouldSubscribe = false, Props},
    {succeed, NewMessage, true, Props}.

succeed(Props, _Msg) ->
    Props.

fail(Props, _Reason, _Message) ->
    Props.

%log(Terms) ->
    %gerlshmud_event_log:log(debug, [?MODULE | Terms]).
