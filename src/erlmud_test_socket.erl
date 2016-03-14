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

-module(erlmud_test_socket).
-behaviour(gen_server).

-export([start/0]).
-export([send/1]).
-export([messages/0]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {conn,
                messages = [] :: [string()]}).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

send(Msg) ->
    gen_server:cast(?MODULE, Msg).

messages() ->
    gen_server:call(?MODULE, messages).

init(_) ->
  {ok, Conn} = supervisor:start_child(erlmud_conn_sup, [self()]),
  {ok, #state{conn = Conn}}.

handle_call(messages, _From, State = #state{messages = Messages}) ->
    {reply, Messages, State};
handle_call(_Req = Text, _From, State = #state{conn = Conn}) ->
    erlmud_conn:handle(Conn, Text),
    {reply, ok, State}.

handle_cast(_Req = Text, State = #state{conn = Conn}) ->
    erlmud_conn:handle(Conn, Text),
    {noreply, ok, State}.

handle_info({send, Msg}, State = #state{messages = Messages}) ->
    {noreply, ok, State#state{messages = [Msg | Messages]}};
handle_info(_Req, State) ->
    {noreply, ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_, _, _) ->
    {error, not_implemented}.
