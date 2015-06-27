-module(erlmud_SUITE).
-compile(export_all).

-define(WORLD_1, [{erlmud_room, room_n, [{exit, exit}, {player, player}]},
                  {erlmud_room, room_s, [{exit, exit}]},
                  {erlmud_player, player, [{room, room_n}]},
                  {erlmud_exit, exit, [{{room, n}, room_n}, {{room, s}, room_s}]}]).

-define(WORLD_2, [{erlmud_room, room, [{player, player}, {item, sword}, {item, apple}]},
                  {erlmud_player, player, [{room, room}, {item, helmet}]},
                  {erlmud_item, sword, [{owner, room}, {name, "sword"}]},
                  {erlmud_item, helmet, [{owner, player}, {name, "helmet"}]},
                  {erlmud_item, apple, [{owner, room}, {name, "apple"}]}]).

-define(WORLD_3, [{erlmud_room, room, [{player, player}, {ai, zombie}]},
                  {erlmud_player, player, [{room, room}, {attack_wait, 10}, {item, fist}]},
                  {erlmud_ai, zombie, [{hp, 10}, {room, room}, {name, "zombie"}]},
                  {erlmud_item, fist, [{dmg, 5}, {owner, player}]}]).

-define(WORLD_4, [{erlmud_room, room, [{player, player}]},
                  {erlmud_player, player, [{room, room},
                                           {item, helmet},
                                           {body_part, head}]},
                  {erlmud_body_part, head, [{name, "head"}, {owner, player}]},
                  {erlmud_item, helmet, [{owner, player}, {name, "helmet"}]}]).

-define(WORLD_5, [{erlmud_player, player, [{item, helmet},
                                           {body_part, head},
                                           {body_part, finger}]},
                  {erlmud_body_part, head, [{name, "head"}, {owner, player}]},
                  {erlmud_body_part, finger, [{name, "finger"}, {owner, player}]},
                  {erlmud_item, helmet, [{owner, player},
                                         {name, "helmet"},
                                         {body_parts, [head, hand]]}]).

all() ->
    [player_move,
     player_move_fail,
     player_get_item,
     player_drop_item,
     player_attack,
     player_attack_wait,
     player_wield,
     player_wield_fail,
     player_remove].
    %[player_remove].

init_per_testcase(_, Config) ->
    {ok, _Started} = application:ensure_all_started(erlmud),
    Config.

end_per_testcase(_, _) ->
    application:stop(erlmud).

val(Key, Obj) ->
    proplists:get_value(Key, get_props(Obj)).

all(Key, Obj) ->
    proplists:get_all_values(Key, get_props(Obj)).

has(Val, Obj) ->
    false /= lists:keyfind(Val, 2, get_props(Obj)).

get_props(Obj) when is_atom(Obj) ->
    get_props(erlmud_index:get(Obj));
get_props(Pid) ->
    {_, _, Props} = sys:get_state(Pid),
    Props.

player_move(_Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_n),
    RoomSouth =  erlmud_index:get(room_s),

    RoomNorth = val(room, Player),
    erlmud_object:attempt(Player, {move, Player, s}),
    receive after 100 -> ok end,
    RoomSouth = val(room, Player).

player_move_fail(_Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_n),
    RoomNorth = val(room, Player),
    erlmud_object:attempt(Player, {move, Player, non_existent_exit}),
    receive after 100 -> ok end,
    RoomNorth = val(room, Player).

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
    receive after 100 -> ok end,
    -2 = val(hp, zombie).

player_attack_wait(_Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    erlmud_object:set(Player, {attack_wait, 1000000}),
    erlmud_object:attempt(Player, {attack, Player, "zombie"}),
    receive after 100 -> ok end,
    4 = val(hp, zombie).

player_wield(_Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    erlmud_object:attempt(Player, {add, Player, "helmet", "head"}),
    receive after 100 -> ok end,
    Helmet = val(item, head).

player_wield_fail(_Config) ->
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
