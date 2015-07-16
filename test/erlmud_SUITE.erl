-module(erlmud_SUITE).
-compile(export_all).

-include("erlmud_test_worlds.hrl").

all() ->
    %[player_move,
     %player_move_fail,
     %player_get_item,
     %player_drop_item,
     %player_attack,
     %player_attack_wait,
     %one_sided_fight,
     %counterattack_behaviour,
     %player_wield,
     %player_wield_missing_body_part,
     %player_wield_wrong_body_part,
     %player_wield_body_part_is_full,
     %player_remove].
    %[player_remove].
    %[player_wield].
    %[player_wield_wrong_body_part].
    [player_attack].
    %[player_attack_wait].
    %[counterattack_behaviour].
    %[player_move_exit_locked].
    %[player_move].

init_per_testcase(_, Config) ->
    {ok, _Started} = application:ensure_all_started(erlmud),
    Config.

end_per_testcase(_, _) ->
    application:stop(erlmud).

val(Key, Obj) ->
    ct:pal("get val for ~p in ~p~n", [Key, Obj]),
    proplists:get_value(Key, get_props(Obj)).

all(Key, Obj) ->
    proplists:get_all_values(Key, get_props(Obj)).

has(Val, Obj) ->
    false /= lists:keyfind(Val, 2, get_props(Obj)).

get_props(Obj) when is_atom(Obj) ->
    ct:pal("get pid for ~p~n", [Obj]),
    get_props(erlmud_index:get(Obj));
get_props(Pid) ->
    ct:pal("get state for pid ~p~n", [Pid]),
    ct:pal("Pid ~p is alive? ~p~n", [Pid, is_process_alive(Pid)]),
    {_, _, Props} = sys:get_state(Pid),
    Props.

player_move(_Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomSouth =  erlmud_index:get(room_s),

    RoomNorth = val(room, Player),
    erlmud_object:attempt(Player, {move, Player, s}),
    receive after 100 -> ok end,
    RoomSouth = val(room, Player).

player_move_fail(_Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomNorth = val(room, Player),
    erlmud_object:attempt(Player, {move, Player, non_existent_exit}),
    receive after 100 -> ok end,
    RoomNorth = val(room, Player).

player_move_exit_locked(_Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomEast =  erlmud_index:get(room_e),
    ExitEastWest =  erlmud_index:get(exit_ew),
    RoomNorth = val(room, Player),
    erlmud_object:attempt(Player, {move, Player, e}),
    receive after 100 -> ok end,
    RoomNorth = val(room, Player),
    erlmud_object:set(ExitEastWest, {locked, false}),
    erlmud_object:attempt(Player, {move, Player, e}),
    receive after 100 -> ok end,
    RoomEast = val(room, Player).

player_get_item(_Config) ->
    start(?WORLD_2),
    Player = erlmud_index:get(player),
    Item = erlmud_index:get(item),
    erlmud_object:attempt(Player, {get, Player, "sword"}),
    receive after 100 -> ok end,
    has(Item, player).

player_drop_item(_Config) ->
    start(?WORLD_2),
    Player = erlmud_index:get(player),
    erlmud_object:attempt(Player, {drop, Player, "helmet"}),
    receive after 100 -> ok end,
    [] = all(item, Player).

player_attack(_Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    erlmud_object:attempt(Player, {attack, Player, "zombie"}),
    receive after 10000 -> ok end,
    false = val(is_alive, z_life),
    0 = val(hitpoints, z_hp).

player_attack_wait(_Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    erlmud_object:set(Player, {attack_wait, 10000}),
    erlmud_object:attempt(Player, {attack, Player, "zombie"}),
    receive after 100 -> ok end,
    5 = val(hitpoints, z_hp),
    true = val(is_alive, z_life),
    true = is_pid(val(attack, Player)).

one_sided_fight(_Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    Zombie = erlmud_index:get(zombie),
    erlmud_object:attempt(Player, {attack, Player, "zombie"}),
    receive after 100 -> ok end,
    1000 = val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    undefined = val(attack, Player),
    0 = val(hitpoints, z_hp),
    false = val(is_alive, z_life),
    undefined = val(attack, Zombie).

counterattack_behaviour(_Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    erlmud_object:set(Player, {attack_wait, 20}),
    Zombie = erlmud_index:get(zombie),
    Behaviour = start_obj(behaviour,
                          erlmud_behaviour,
                          [{owner, Zombie},
                           {attack_wait, 10}]),
    erlmud_object:set(Zombie, {behaviour, Behaviour}),
    erlmud_object:attempt(Player, {attack, Player, "zombie"}),
    receive after 100 -> ok end,
    true = 1000 > val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    undefined = val(attack, Player),
    0 = val(hitpoints, z_hp),
    false = val(is_alive, z_life),
    undefined = val(attack, Zombie).

player_wield(_Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    erlmud_object:attempt(Player, {add, Player, "helmet", "head"}),
    receive after 100 -> ok end,
    Helmet = val(item, head).

player_wield_missing_body_part(_Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    erlmud_object:attempt(Player, {add, Player, "helmet", "finger"}),
    receive after 100 -> ok end,
    undefined = val(item, head),
    Helmet = val(item, player),
    erlmud_object:attempt(Player, {add, Player, "helmet", "head"}),
    receive after 100 -> ok end,
    Helmet = val(item, head),
    undefined = val(item, player).

player_wield_wrong_body_part(_Config) ->
    start(?WORLD_5),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    erlmud_object:attempt(Player, {add, Player, "helmet", "finger"}),
    receive after 100 -> ok end,
    undefined = val(item, head1),
    Helmet = val(item, player),
    erlmud_object:attempt(Player, {add, Player, "helmet", "head"}),
    receive after 100 -> ok end,
    Helmet = val(item, head1),
    undefined = val(item, player).

player_wield_body_part_is_full(_Config) ->
    start(?WORLD_6),
    Player = erlmud_index:get(player),
    Ring1 = erlmud_index:get(ring1),
    Ring2 = erlmud_index:get(ring2),
    [Ring1, Ring2] = all(item, player),
    [] = all(item, finger1),
    [] = all(item, finger2),
    erlmud_object:attempt(Player, {add, Player, "ring1", "finger1"}),
    receive after 100 -> ok end,
    [Ring2] = all(item, player),
    [Ring1] = all(item, finger1),
    [] = all(item, finger2),
    erlmud_object:attempt(Player, {add, Player, "ring2", "finger1"}),
    receive after 100 -> ok end,
    [Ring2] = all(item, player),
    [Ring1] = all(item, finger1),
    [] = all(item, finger2),
    erlmud_object:attempt(Player, {add, Player, "ring2"}),
    receive after 100 -> ok end,
    [] = all(item, player),
    [Ring1] = all(item, finger1),
    [Ring2] = all(item, finger2).

player_remove(_Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    erlmud_object:attempt(Player, {add, Player, "helmet", "head"}),
    receive after 100 -> ok end,
    undefined = val(item, player),
    Helmet = val(item, head),
    erlmud_object:attempt(Player, {remove, Player, "helmet", "head"}),
    receive after 100 -> ok end,
    Helmet = val(item, player),
    undefined = val(item, head),
    erlmud_object:attempt(Player, {add, Player, "helmet", "head"}),
    receive after 100 -> ok end,
    undefined = val(item, player),
    Helmet = val(item, head),
    erlmud_object:attempt(Player, {remove, Player, "helmet"}),
    receive after 100 -> ok end,
    Helmet = val(item, player),
    undefined = val(item, head).

start(Objects) ->
    IdPids = [{Id, start_obj(Id, Type, Props)} || {Type, Id, Props} <- Objects],
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids].

start_obj(Id, Type, Props) ->
    {ok, Pid} = supervisor:start_child(erlmud_object_sup, [Id, Type, Props]),
    Pid.
