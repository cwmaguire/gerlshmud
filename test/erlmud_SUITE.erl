-module(erlmud_SUITE).
-compile(export_all).

-define(OBJECTS, [{erlmud_room, room1, [{exit, exit1},
                                        {item, item1},
                                        {ai, ai1},
                                        {player, player1}]},
                  {erlmud_room, room2, [{exit, exit1}, {exit, exit2}]},
                  {erlmud_room, room3, [{exit, exit2}]},
                  {erlmud_exit, exit1, [{{room, s}, room2}, {{room, n}, room1}]},
                  {erlmud_exit, exit2, [{{room, n}, room2}, {{room, s}, room3}]},
                  {erlmud_player, player, [{room, room1}, {item, item2}, {item, fist1}, {attack_wait, 10}]},
                  {erlmud_item, item1, [{name, "sword"}, {owner, room1}]},
                  {erlmud_item, item2, [{name, "helmet"}, {owner, player}]},
                  {erlmud_ai, ai1, [{hp, 10}, {room, room1}, {name, "zombie"}]},
                  {erlmud_item, fist1, [{dmg, 5}, {owner, player}]}]).
all() ->
    [player_move,
     player_move_fail,
     player_get_item,
     player_drop_item,
     player_attack
    ].

init_per_testcase(_, Config) ->
    {ok, _Started} = application:ensure_all_started(erlmud),
    Config.

end_per_testcase(_, _) ->
    application:stop(erlmud).

player_move(_Config) ->
    start(),
    Player = erlmud_index:get(player),
    Room2 =  erlmud_index:get(room2),
    gen_server:cast(Player, {attempt, {move, Player, s}, {procs, undefined, [], [], []}}),
    receive after 100 -> ok end,
    {_, _, PlayerProps} = sys:get_state(erlmud_index:get(player)),
    Room2 = proplists:get_value(room, PlayerProps).

player_move_fail(_config) ->
    start(),
    Player = erlmud_index:get(player),
    Room1 =  erlmud_index:get(room1),
    gen_server:cast(Player, {attempt, {move, Player, non_existent_exit}, {procs, undefined, [], [], []}}),
    receive after 100 -> ok end,
    {_, _, PlayerProps} = sys:get_state(erlmud_index:get(player)),
    Room1 = proplists:get_value(room, PlayerProps).

player_get_item(_Config) ->
    start(),
    Player = erlmud_index:get(player),
    Item = erlmud_index:get(item1),
    gen_server:cast(Player, {attempt, {get, Player, "sword"}, {procs, undefined, [], [], []}}),
    receive after 100 -> ok end,
    {_, _, PlayerProps} = sys:get_state(erlmud_index:get(player)),
    true = lists:member(Item, proplists:get_all_values(item, PlayerProps)).

player_drop_item(_Config) ->
    start(),
    Player = erlmud_index:get(player),
    gen_server:cast(Player, {attempt, {drop, Player, "helmet"}, {procs, undefined, [], [], []}}),
    receive after 100 -> ok end,
    {_, _, PlayerProps} = sys:get_state(erlmud_index:get(player)),
    [] = proplists:get_all_values(item, PlayerProps).

player_attack(_Config) ->
    start(),
    Player = erlmud_index:get(player),
    gen_server:cast(Player, {attempt, {attack, Player, "zombie"}, {procs, undefined, [], [], []}}),
    receive after 1000 -> ok end,
    {_, _, AIProps} = sys:get_state(erlmud_index:get(ai1)),
    -2 = proplists:get_value(hp, AIProps).

start() ->
    IdPids = [{Id, start_obj(Id, Type, Props)} || {Type, Id, Props} <- ?OBJECTS],
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids].

start_obj(Id, Type, Props) ->
    {ok, Pid} = supervisor:start_child(erlmud_object_sup, [Id, Type, Props]),
    Pid.
