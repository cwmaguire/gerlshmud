-module(erlmud_world).

-export([init/0]).
-export([move/1]).
-export([m/0]).
-export([s/0]).

-define(WORLD,
        [{erlmud_room, room1, [{player, player1}, {exit, exit1}]},
         {erlmud_room, room2, [{exit, exit1}]},
         {erlmud_player, player1, [{room, room1}]},
         {erlmud_exit, exit1, [{{room, s}, room2}, {{room, n}, room1}]},
         {erlmud_item, item1, [{desc, "sword"}]}]).

init() ->
    IdPids = [{Id, start(Id, Type, Props)} || {Type, Id, Props} <- ?WORLD],
    io:format("Pids: ~p~n", [IdPids]),
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids],
    IdPids.

start(Id, Type, Props) ->
    {ok, Pid} = supervisor:start_child(erlmud_sup, [Id, Type, Props]),
    Pid.

move(IdPids) ->
    Room1 = proplists:get_value(room1, IdPids),
    Room2 = proplists:get_value(room2, IdPids),
    Player1 = proplists:get_value(player1, IdPids),
    Procs = {[], [], []},
    gen_server:cast(Room1, {attempt, {move, Player1, Room1, Room2}, Procs}).

m() ->
    Pids = init(),
    Player1 = proplists:get_value(player1, Pids),
    Room1 = proplists:get_value(room1, Pids),
    Room2 = proplists:get_value(room2, Pids),
    gen_server:cast(Room1, {attempt, {move, Player1, Room1, Room2}, {[], [], []}}).

s() ->
    [io:format("~p~n", [st(X)]) || X <- [player1, room1, room2, exit1, item1]].

st(Regname) ->
    sys:get_status(Regname).
