%% Route drawing commands to and from a web page to an Erlang process
%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_websocket_log).
-behaviour(cowboy_http_handler).

-include_lib("kernel/include/logger.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([handle/2]).
-export([terminate/3]).

init(_, _Req, _Opts) ->
    io:format("Websocket log handler init (~p)~n", [self()]),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
    ?LOG_INFO("Websocket log handler websocket_init (~p) start~n", [self()]),
    Req3 = case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        {ok, undefined, Req2} ->
            Req2;
        {ok, Subprotocols, Req2} ->
            ?LOG_INFO("Subprotocols found: ~p~n", [Subprotocols]),
            Req2
    end,

    Self = self(),
    Fun =
    fun(_Level, JSON) ->
        Self ! JSON
    end,
    gerlshmud_event_log:register(Fun),

    ?LOG_INFO("Websocket handler websocket_init end (~p)~n", [self()]),
    {ok, Req3, undefined}.

websocket_handle(X, Req, State) ->
    ?LOG_INFO("Received ~p~n", X),
    {ok, Req, State}.

websocket_info({send, Log}, Req, State) ->
    ?LOG_INFO("Sending log message: ~p~n", [Log]),
    {reply, {text, [Log]}, Req, State};
websocket_info(ErlangMessage, Req, State) ->
    ?LOG_INFO("websocket_info(~p, ~p, ~p)~n", [ErlangMessage, Req, State]),
    {reply, {text, [ErlangMessage]}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
