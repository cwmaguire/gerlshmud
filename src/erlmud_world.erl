-module(erlmud_world).

-export([init/0]).
-export([move/1]).

-define(WORLD,
        [{erlmud_room, room1, [{players, [player1]}]},
         {erlmud_room, room2, [{players, [player1]}]},
         {erlmud_player, player1, [{room, room1}]},
         {erlmud_exit, exit1, [{s, room2}, {n, room1}]}]).

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
    %Exit1 = proplists:get_value(exit1, IdPids),
    gen_server:cast(Room1, {attempt, {move, Player1, Room1, Room2}, [], []}).
