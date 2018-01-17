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
-behaviour(gen_statem).

-export([start_link/1]).
-export([handle/2]).

-export([login/3]).
-export([password/3]).
-export([live/3]).
-export([dead/3]).

-export([init/1]).
-export([callback_mode/0]).

-record(data, {socket :: pid(),
               conn_obj :: pid(),
               player :: pid(),
               login :: string(),
               attempts = 0 :: integer()}).

%% api

start_link(Socket) ->
    gen_statem:start_link(?MODULE, Socket, []).

handle(Pid, Msg) ->
    gen_statem:cast(Pid, Msg).

%% states

login(cast, Event, Data) ->
    {next_state, password, Data#data{login = Event}}.

password(cast, _Event = Password, Data = #data{login = Login,
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

            {next_state, live, Data#data{login = undefined, player = PlayerPid, conn_obj = ConnObjPid}};
        false ->
            get_failed_auth_state(Data#data{login = undefined, attempts = Attempts + 1})
    end.

get_failed_auth_state(Data = #data{attempts = Attempts}) when Attempts < 3 ->
    {next_state, login, Data};
get_failed_auth_state(Data) ->
    {next_state, dead, Data}.

dead(cast, _, _Data = #data{socket = Socket}) ->
    Socket ! {send, "Connection Refused"},
    keep_state_and_data;

dead({call, From}, props, _Data) ->
    {keep_state_and_data, [{reply, From, _Props = []}]}.

live(cast, {send, Message}, _Data = #data{socket = Socket}) ->
    Socket ! {send, Message},
    keep_state_and_data;
live(cast, Event, Data = #data{player = PlayerPid, conn_obj = ConnObjPid}) ->
    log([<<"got event \"">>, Event, <<"\" in state 'live' with state data ">>, Data]),

    _ = case erlmud_parse:parse(PlayerPid, Event) of
        {error, Error} ->
            Data#data.socket ! {send, Error};
        Message ->
            ConnObjPid ! {ConnObjPid, Message}
    end,
    {next_state, live, Data};
live(Type, Event, Data) ->
    log(live, Type, Event, Data),
    keep_state_and_data.

%% gen_statem

init(Socket) ->
    {ok, login, #data{socket = Socket}}.

callback_mode() ->
    state_functions.

log(EventType, EventData, State, _Data = #data{player = Player}) ->
    io:format("Connection ~p for player ~p received unrecognized event ~p:~p in state ~p",
              [self(), Player, EventType, EventData, State]).

%% private

is_valid_creds(_String, never_fails) ->
    false;
is_valid_creds(_Login, _Password) ->
    {true, erlmud_index:get(player)}.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
