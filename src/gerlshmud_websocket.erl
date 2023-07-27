%% Route drawing commands to and from a web page to an Erlang process
%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_websocket).
-behaviour(cowboy_handler).

-include_lib("kernel/include/logger.hrl").

-export([init/2]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {conn :: pid()}).

init(Req, State) ->
    io:format("Websocket handler init (~p)~n", [self()]),
    {cowboy_websocket, Req, State}.

websocket_init(_Type, Req, _Opts) ->
    ?LOG_INFO("Websocket handler websocket_init (~p) start~n", [self()]),
    Req3 = case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        {ok, undefined, Req2} ->
            Req2;
        {ok, Subprotocols, Req2} ->
            ?LOG_INFO("Subprotocols found: ~p~n", [Subprotocols]),
            Req2
    end,

    {ok, Conn} = supervisor:start_child(gerlshmud_conn_sup, [self()]),

    ?LOG_INFO("Websocket handler websocket_init end (~p)~n", [self()]),
    {ok, Req3, #state{conn = Conn}}.

websocket_handle({text, Text}, Req, State = #state{conn = Conn}) ->
    ?LOG_INFO("From Websocket: {~p, ~p}~n", [text, Text]),
    gerlshmud_conn:handle(Conn, Text),
    {ok, Req, State};
websocket_handle({FrameType, FrameContent}, Req, State) ->
    ?LOG_INFO("From Websocket: {~p, ~p}~n", [FrameType, FrameContent]),
    {ok, Req, State};
websocket_handle(X, Req, State) ->
    ?LOG_INFO("Received ~p~n", X),
    {ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
    ?LOG_INFO("Sending message: ~p~n", [Msg]),
    {reply, {text, [Msg]}, Req, State};
websocket_info(ErlangMessage, Req, State) ->
    ?LOG_INFO("websocket_info(~p, ~p, ~p)~n", [ErlangMessage, Req, State]),
    {reply, {text, [ErlangMessage]}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, State=#state{}) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
