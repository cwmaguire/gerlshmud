%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% Route drawing commands to and from a web page to an Erlang process
-module(erlmud_websocket).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {conn :: pid()}).

init(_, _Req, _Opts) ->
    io:format("Websocket handler init (~p)~n", [self()]),
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
    lager:info("Websocket handler websocket_init (~p) start~n", [self()]),
    Req3 = case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        {ok, undefined, Req2} ->
            Req2;
        {ok, Subprotocols, Req2} ->
            lager:info("Subprotocols found: ~p~n", [Subprotocols]),
            Req2
    end,

    {ok, Conn} = supervisor:start_child(erlmud_conn_sup, [self()]),

    lager:info("Websocket handler websocket_init end (~p)~n", [self()]),
    {ok, Req3, #state{conn = Conn}}.

websocket_handle({text, Text}, Req, State = #state{conn = Conn}) ->
    lager:info("From Websocket: {~p, ~p}~n", [text, Text]),
    erlmud_conn:handle(Conn, Text),
    {ok, Req, State};
websocket_handle({FrameType, FrameContent}, Req, State) ->
    lager:info("From Websocket: {~p, ~p}~n", [FrameType, FrameContent]),
    {ok, Req, State};
websocket_handle(X, Req, State) ->
    lager:info("Received ~p~n", X),
    {ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
    lager:info("Sending message: ~p~n", [Msg]),
    {reply, {text, [Msg]}, Req, State};
websocket_info(ErlangMessage, Req, State) ->
    lager:info("websocket_info(~p, ~p, ~p)~n", [ErlangMessage, Req, State]),
    {reply, {text, [ErlangMessage]}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

handle(Req, State=#state{}) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.
