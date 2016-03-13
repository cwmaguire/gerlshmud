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
-module(erlmud_conn).
%-behaviour(erlmud_connection).
-behaviour(erlmud_object).
-behaviour(gen_fsm).

-export([start_link/1]).
-export([handle/2]).

-export([login/2]).
-export([password/2]).
-export([live/2]).
-export([dead/2]).

-export([init/1]).
-export([handle_event/3]).
-export([handle_sync_event/4]).
-export([handle_info/3]).
-export([terminate/3]).
-export([code_change/4]).

-record(state, {socket :: pid(),
                player :: pid(),
                login :: string(),
                room :: pid(),
                attempts = 0 :: integer()}).

%% api

start_link(Socket) ->
    gen_fsm:start_link(?MODULE, Socket, []).

handle(Pid, Msg) ->
    gen_fsm:send_event(Pid, Msg).

%% states

login(Event, StateData) ->
    {next_state, password, StateData#state{login = Event}}.

password(Event, StateData = #state{login = Login,
                                   attempts = Attempts,
                                   socket = _Socket}) ->
    case is_valid_creds(Login, Event) of
        {true, Player} ->
            %% start player
            %% start connection object
            erlmud_object_sup:start_child(erlmud_object_sup, [Id, erlmud_conn_obj, Props]),
            {next_state, live, StateData#state{login = undefined, player = Player}};
        false ->
            get_failed_auth_state(StateData#state{login = undefined, attempts = Attempts + 1})
    end.

get_failed_auth_state(StateData = #state{attempts = Attempts}) when Attempts < 3 ->
    {next_state, login, StateData};
get_failed_auth_state(StateData) ->
    {next_state, dead, StateData}.

dead(_, StateData = #state{socket = Socket}) ->
    Socket ! {send, "Connection Refused"},
    {next_state, dead, StateData}.

live(Event, StateData = #state{player = Player}) ->
    io:format("erlmud_conn got event ~p in state 'live' with state data ~p~n",
              [Event, StateData]),
    _ = case erlmud_parse:parse(Player, Event) of
        {error, Error} ->
            StateData#state.socket ! {send, Error};
        Message ->
            erlmud_object:attempt(Player, Message)
    end,
    {next_state, live, StateData}.

%% gen_fsm

init(Socket) ->
    {ok, login, #state{socket = Socket}}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

handle_event(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info({'$gen_cast', {fail, Reason, {enter_world, Player}}},
            _StateName,
            StateData = #state{player = Player, socket = Socket}) ->
    Socket ! {send, Reason},
    {next_state, dead, StateData};
handle_info({'$gen_cast', {succeed, {enter_world, Player}}},
            StateName,
            StateData = #state{player = Player, socket = Socket}) ->
    erlmud_object:add(Player, conn, self()),
    Socket ! {send, "Login succeeded"},
    {next_state, StateName, StateData};
handle_info({'$gen_cast', {fail, Reason, {logout, Player}}},
            StateName,
            StateData = #state{player = Player, socket = Socket}) ->
    Socket ! {send, Reason},
    {next_state, StateName, StateData};
handle_info({'$gen_cast', {succeed, {logout, Player}}},
            _StateName,
            StateData = #state{player = Player, socket = Socket}) ->
    Socket ! {send, "Logout successful"},
    {next_state, dead, StateData};
handle_info({'$gen_cast', {succeed, {send, Player, Msg}}},
            _StateName,
            StateData = #state{player = Player, socket = Socket}) ->
    Socket ! {send, Msg},
    {next_state, dead, StateData};
handle_info({'$gen_cast', {succeed, {send, Player, Msg}}},
            _StateName,
            StateData = #state{player = Player, socket = Socket}) ->
    Socket ! {send, Msg},
    {next_state, dead, StateData};
handle_info(Info, StateName, StateData = #state{player = Player}) ->
    io:format("Connection for player ~p received unrecognized message:~n\t~p~n",
              [Player, Info]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% private

is_valid_creds(_String, never_fails) ->
    false;
is_valid_creds(_Login, _Password) ->
    {true, erlmud_index:get(player)}.
