-module(erlmud_world).

-export([init/0]).

-define(WORLD,
        [{erlmud_room, room1, [{players, [player1]}]},
         {erlmud_room, room2, [{players, [player1]}]},
         {erlmud_player, player1, [{room, room1}]},
         {erlmud_exit, exit1, [{s, room2}, {n, room1}]}]).

init() ->
    IdPids = [{Id, start(Type, Props)} || {Type, Id, Props} <- ?WORLD],
    io:format("Pids: ~p~n", [IdPids]),
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids].

start(Type, Props) ->
    {ok, Pid} = supervisor:start_child(erlmud_sup, [Type, Props]),
    Pid.
