-module(erlmud_world).

-export([init/0]).

-define(WORLD,
        [{erlmud_room, room1, [{players, [player1]}]},
         {erlmud_player, player1, [{room, room1}]}]).

init() ->
    IdPids = [{Id, erlmud_object:start_link(Type, Props)} || {Type, Id, Props} <- ?WORLD],
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids].
