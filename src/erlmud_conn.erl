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
                conn_obj :: pid(),
                player :: pid(),
                login :: string(),
                attempts = 0 :: integer()}).

%% api

start_link(Socket) ->
    gen_fsm:start_link(?MODULE, Socket, []).

handle(Pid, Msg) ->
    gen_fsm:send_event(Pid, Msg).

%% states

login(Event, StateData) ->
    {next_state, password, StateData#state{login = Event}}.

password(_Event = Password, StateData = #state{login = Login,
                                               attempts = Attempts,
                                               socket = _Socket}) ->
    case is_valid_creds(Login, Password) of
        {true, _Player} ->
            % All players are live processes at MUD startup; processes are almost free
            PlayerPid = erlmud_index:get(player),
            ConnProps = [{owner, PlayerPid},
                         {conn, {self()}},
                         {handlers, [erlmud_handler_conn_enter_world,
                                     erlmud_handler_conn_send]}],

            {ok, ConnObjPid} = supervisor:start_child(erlmud_object_sup, [_Id = undefined, ConnProps]),
            Message = {PlayerPid, enter_world, with, ConnObjPid},
            ConnObjPid ! {ConnObjPid, Message},

            {next_state, live, StateData#state{login = undefined, player = PlayerPid, conn_obj = ConnObjPid}};
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

live({send, Message}, StateData = #state{socket = Socket}) ->
    Socket ! {send, Message},
    {next_state, live, StateData};
live(Event, StateData = #state{player = PlayerPid, conn_obj = ConnObjPid}) ->
    log([<<"got event \"">>, Event, <<"\" in state 'live' with state data ">>, StateData]),

    _ = case erlmud_parse:parse(PlayerPid, Event) of
        {error, Error} ->
            StateData#state.socket ! {send, Error};
        Message ->
            ConnObjPid ! {ConnObjPid, Message}
    end,
    {next_state, live, StateData}.

%% gen_fsm

init(Socket) ->
    {ok, login, #state{socket = Socket}}.

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

handle_event(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_info({'$gen_call', {From, Ref}, props}, StateName, StateData) ->
    From ! {Ref, _Props = []},
    {next_state, StateName, StateData};
handle_info(Info, StateName, StateData = #state{player = Player}) ->
    io:format("Connection ~p for player ~p received unrecognized message:~n\t~p~n",
              [self(), Player, Info]),
    {next_state, StateName, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% private

is_valid_creds(_String, never_fails) ->
    false;
is_valid_creds(_Login, _Password) ->
    {true, erlmud_index:get(player)}.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
