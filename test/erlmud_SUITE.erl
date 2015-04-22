-module(erlmud_SUITE).
-compile(export_all).

-define(OBJECTS, [{erlmud_room, room1, [{exit, exit1}, {item, item}]},
                  {erlmud_room, room2, [{exit, exit1}, {exit, exit2}]},
                  {erlmud_room, room3, [{exit, exit2}]},
                  {erlmud_exit, exit1, [{{room, s}, room2}, {{room, n}, room1}]},
                  {erlmud_exit, exit2, [{{room, n}, room2}, {{room, s}, room3}]},
                  {erlmud_player, player, [{room, room1}]},
                  {erlmud_item, item, [{name, "sword"}]}]).
all() ->
    [player_move,
     player_get_item].

init_per_testcase(_, Config) ->
    ct:pal("Starting erlmud~n"),
    {ok, _Started} = application:ensure_all_started(erlmud),
    Config.

end_per_testcase(_, _) ->
    ct:pal("Shutting down erlmud~n"),
    application:stop(erlmud).

player_move(_Config) ->
    IdPids = [{Id, start(Id, Type, Props)} || {Type, Id, Props} <- ?OBJECTS],
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids],
    Player = whereis(player),
    Room2 = whereis(room2),
    gen_server:cast(Player, {attempt, {move, Player, s}, {procs, undefined, [], [], []}}),
    receive after 100 -> ok end,
    {_, _, PlayerProps} = sys:get_state(player),
    Room2 = proplists:get_value(room, PlayerProps).

player_get_item(_Config) ->
    IdPids = [{Id, start(Id, Type, Props)} || {Type, Id, Props} <- ?OBJECTS],
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids],
    Player = whereis(player),
    Item = whereis(item),
    gen_server:cast(Player, {attempt, {get, Player, "sword"}, {procs, undefined, [], [], []}}),
    receive after 100 -> ok end,
    {_, _, PlayerProps} = sys:get_state(player),
    ct:pal("Player props: ~p~n", [PlayerProps]),
    Item = proplists:get_value(item, PlayerProps).


start(Id, Type, Props) ->
    {ok, Pid} = supervisor:start_child(erlmud_object_sup, [Id, Type, Props]),
    Pid.
