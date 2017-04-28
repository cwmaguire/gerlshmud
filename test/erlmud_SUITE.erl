-module(erlmud_SUITE).
-compile(export_all).

-include("erlmud_test_worlds.hrl").

-define(WAIT100, receive after 100 -> ok end).

% TODO test cancelling an attack by moving

%all() -> [player_attack].
all() -> [attack_with_modifiers].
%all() ->
    %[player_move,
     %player_move_fail,
     %player_move_exit_locked,
     %player_get_item,
     %player_drop_item,
     %character_owner_add_remove,
     %player_attack,
     %player_resource_wait,
     %attack_with_modifiers,
     %one_sided_fight,
     %counterattack_behaviour,
     %player_wield,
     %player_wield_first_available,
     %player_wield_missing_body_part,
     %player_wield_wrong_body_part,
     %player_wield_body_part_is_full,
     %player_remove,
     %look_player,
     %look_room,
     %look_item,
     %set_character].

init_per_testcase(_, Config) ->
    {ok, _Started} = application:ensure_all_started(erlmud),
    {ok, _Pid} = erlmud_test_socket:start(),
    TestObject = spawn_link(fun mock_object/0),
    ct:pal("mock_object is ~p~n", [TestObject]),
    erlmud_index:put("TestObject", TestObject),
    [{test_object, TestObject} | Config].

end_per_testcase(_, _Config) ->
    ct:pal("~p stopping erlmud~n", [?MODULE]),
    erlmud_test_socket:stop(),
    application:stop(erlmud).

val(Key, Obj) ->
    Props = case get_props(Obj) of
                undefined ->
                    ct:pal("erlmud_SUITE:val/2: props for ~p undefined", [Obj]),
                    [];
                Props_ ->
                    Props_
            end,
    Val = proplists:get_value(Key, Props),
    ct:pal("erlmud_SUITE:val/2: ~p for ~p is ~p~n", [Key, Obj, Val]),
    Val.

all(Key, Obj) ->
    proplists:get_all_values(Key, get_props(Obj)).

has(Val, Obj) ->
    false /= lists:keyfind(Val, 2, get_props(Obj)).

get_props(undefined) ->
    [];
get_props(Obj) when is_atom(Obj) ->
    Pid = erlmud_index:get(Obj),
    get_props(Pid);
get_props(Pid) when is_pid(Pid) ->
    {_RecordName, Props} = sys:get_state(Pid),
    Props.

player_move(Config) ->
    %erlmud_dbg:add(erlmud_object),
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomSouth =  erlmud_index:get(room_s),

    RoomNorth = val(owner, Player),
    attempt(Config, Player, {move, Player, s}),
    ?WAIT100,
    RoomSouth = val(owner, Player).

player_move_fail(Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomNorth = val(owner, Player),
    attempt(Config, Player, {move, Player, non_existent_exit}),
    ?WAIT100,
    RoomNorth = val(owner, Player).

player_move_exit_locked(Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomEast =  erlmud_index:get(room_e),
    ExitEastWest =  erlmud_index:get(exit_ew),
    RoomNorth = val(owner, Player),
    attempt(Config, Player, {move, Player, e}),
    ?WAIT100,
    RoomNorth = val(owner, Player),
    erlmud_object:set(ExitEastWest, {is_locked, false}),
    attempt(Config, Player, {move, Player, e}),
    ?WAIT100,
    RoomEast = val(owner, Player).

player_get_item(Config) ->
    start(?WORLD_2),
    Player = erlmud_index:get(player),
    Sword = erlmud_index:get(sword),
    attempt(Config, Player, {Player, get, <<"sword">>}),
    ?WAIT100,
    true = has(Sword, player).

player_drop_item(Config) ->
    start(?WORLD_2),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    true = has(Helmet, player),
    attempt(Config, Player, {Player, drop, <<"helmet">>}),
    ?WAIT100,
    [] = all(item, Player).

character_owner_add_remove(Config) ->
    start(?WORLD_10),
    Player = erlmud_index:get(player),
    Rifle = erlmud_index:get(rifle),
    Suppressor = erlmud_index:get(suppressor),
    Grip = erlmud_index:get(grip),
    Clip = erlmud_index:get(clip),
    Bullet = erlmud_index:get(bullet),
    attempt(Config, Player, {Player, get, <<"rifle">>}),
    ?WAIT100,
    true = has(Rifle, player),
    Player = val(character, Rifle),
    Player = val(character, Suppressor),
    Player = val(character, Grip),
    Player = val(character, Clip),
    Player = val(character, Bullet),
    attempt(Config, Player, {Player, drop, <<"rifle">>}),
    ?WAIT100,
    false = has(Rifle, player),
    undefined = val(character, Rifle),
    undefined = val(character, Suppressor),
    undefined = val(character, Grip),
    undefined = val(character, Clip),
    undefined = val(character, Bullet).

player_attack(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    receive after 1000 -> ok end,
    false = val(is_alive, z_life),
    0 = val(hitpoints, z_hp).

player_resource_wait(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    Fist = erlmud_index:get(p_fist),
    Stamina = erlmud_index:get(p_stamina),
    Zombie = erlmud_index:get(zombie),
    erlmud_object:set(Stamina, {current, 5}),
    erlmud_object:set(Stamina, {tick_time, 10000}),
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    ?WAIT100,
    5 = val(hitpoints, z_hp),
    true = val(is_alive, z_life),
    true = val(is_attacking, p_fist),
    Zombie = val(target, Fist).

one_sided_fight(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    Zombie = erlmud_index:get(zombie),
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    ?WAIT100,
    ?WAIT100,
    1000 = val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    undefined = val(attack, Player),
    0 = val(hitpoints, z_hp),
    false = val(is_alive, z_life),
    undefined = val(attack, Zombie).

counterattack_behaviour(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    erlmud_object:set(Player, {attack_wait, 20}),
    Zombie = erlmud_index:get(zombie),
    Handlers = val(handlers, zombie),
    ct:pal("Zombie handlers: ~n\t~p~n", [Handlers]),
    erlmud_object:set(Zombie, {handlers, [erlmud_handler_counterattack | Handlers]}),
    ?WAIT100,
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    ?WAIT100,
    ?WAIT100,
    true = 1000 > val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    undefined = val(attack, Player),
    0 = val(hitpoints, z_hp),
    false = val(is_alive, z_life),
    undefined = val(attack, Zombie),
    ok.

%% TODO make sure that the giant is counterattacking
%% It works though.
attack_with_modifiers(Config) ->
    start(?WORLD_8),
    Room = erlmud_index:get(room),
    Player = erlmud_index:get(player),
    Giant = erlmud_index:get(giant),
    ?WAIT100,
    attempt(Config, Player, {move, <<"force field">>, from, Room, to, Player}),
    attempt(Config, Player, {move, <<"shield">>, from, Room, to, Player}),
    ?WAIT100,
    attempt(Config, Player, {move, <<"force field">>, from, Player, to, first_available_body_part}),
    attempt(Config, Player, {move, <<"shield">>, from, Player, to, first_available_body_part}),
    ?WAIT100,
    attempt(Config, Player, {Player, attack, <<"pete">>}),
    ?WAIT100,
    ?WAIT100,
    timer:sleep(5000),
    %% The giant shouldn't be able to attack the player at all,
    %% so the giant should die and the player should be alive.
    10 = val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    WaitFun =
        fun() ->
            case val(hitpoints, g_hp) of
                ZeroOrLess when is_integer(ZeroOrLess), ZeroOrLess =< 0 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun, true, 30),
    false = val(is_alive, g_life),
    undefined = val(attack, Giant),
    ok.

wait_loop(Fun, ExpectedResult, _Count = 0) ->
    ct:pal("Mismatched function result:~n\tFunction: ~p~n\tResult: ~p",
           [erlang:fun_to_list(Fun), ExpectedResult]),
    false;
wait_loop(Fun, ExpectedResult, Count) ->
    case Fun() == ExpectedResult of
        true ->
            true;
        false ->
            ?WAIT100,
            wait_loop(Fun, ExpectedResult, Count - 1)
    end.

player_wield(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    Head = erlmud_index:get(head1),
    Helmet = val(item, Player),
    attempt(Config, Player, {move, <<"helmet">>, from, Player, to, <<"head">>}),
    ?WAIT100,
    undefined = val(item, Player),
    Helmet = val(item, head1),
    Head = val(body_part, Helmet).

player_wield_first_available(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Head = erlmud_index:get(head1),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {move, <<"helmet">>, from, Player, to, first_available_body_part}),
    ?WAIT100,
    undefined = val(item, Player),
    Helmet = val(item, head1),
    Head = val(body_part, Helmet).

player_wield_missing_body_part(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Head = erlmud_index:get(head1),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {move, <<"helmet">>, from, Player, to, <<"finger">>}),
    ?WAIT100,
    undefined = val(item, head1),
    Helmet = val(item, player),
    attempt(Config, Player, {move, <<"helmet">>, from, Player, to, <<"head">>}),
    ?WAIT100,
    Helmet = val(item, head1),
    Head = val(body_part, Helmet),
    undefined = val(item, player).

player_wield_wrong_body_part(Config) ->
    start(?WORLD_5),
    Player = erlmud_index:get(player),
    Head = erlmud_index:get(head1),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {move, <<"helmet">>, from, Player, to, <<"finger">>}),
    ?WAIT100,
    undefined = val(item, head1),
    Helmet = val(item, player),
    attempt(Config, Player, {move, <<"helmet">>, from, Player, to, <<"head">>}),
    ?WAIT100,
    Helmet = val(item, head1),
    Head = val(body_part, Helmet),
    undefined = val(item, player).

player_wield_body_part_is_full(Config) ->
    start(?WORLD_6),
    Player = erlmud_index:get(player),
    Finger1 = erlmud_index:get(finger1),
    Finger2 = erlmud_index:get(finger2),
    Ring1 = erlmud_index:get(ring1),
    Ring2 = erlmud_index:get(ring2),
    [Ring1, Ring2] = all(item, player),
    [] = all(item, finger1),
    [] = all(item, finger2),
    attempt(Config, Player, {move, <<"ring1">>, from, Player, to, <<"finger1">>}),
    ?WAIT100,
    [Ring2] = all(item, player),
    [Ring1] = all(item, finger1),
    Finger1 = val(body_part, Ring1),
    [] = all(item, finger2),
    attempt(Config, Player, {move, <<"ring2">>, from, Player, to, <<"finger1">>}),
    ?WAIT100,
    [Ring2] = all(item, player),
    [Ring1] = all(item, finger1),
    [] = all(item, finger2),
    attempt(Config, Player, {move, <<"ring2">>, from, Player, to, first_available_body_part}),
    ?WAIT100,
    [] = all(item, player),
    [Ring1] = all(item, finger1),
    [Ring2] = all(item, finger2),
    Finger2 = val(body_part, Ring2).

player_remove(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Head = erlmud_index:get(head1),
    Helmet = erlmud_index:get(helmet),
    DexBuff = erlmud_index:get(dex_buff),
    attempt(Config, Player, {move, <<"helmet">>, from, Player, to, <<"head">>}),
    ?WAIT100,
    undefined = val(item, player),
    Helmet = val(item, head1),
    Head = val(body_part, Helmet),
    Head = val(body_part, DexBuff),
    attempt(Config, Player, {move, <<"helmet">>, from, <<"head">>, to, Player}),
    ?WAIT100,
    Helmet = val(item, player),
    undefined = val(body_part, Helmet),
    undefined = val(body_part, DexBuff),
    undefined = val(item, head1),
    attempt(Config, Player, {move, <<"helmet">>, from, Player, to, <<"head">>}),
    ?WAIT100,
    undefined = val(item, player),
    Helmet = val(item, head1),
    Head = val(body_part, Helmet),
    Head = val(body_part, DexBuff),
    attempt(Config, Player, {move, <<"helmet">>, from, current_body_part, to, Player}),
    ?WAIT100,
    Helmet = val(item, player),
    undefined = val(body_part, Helmet),
    undefined = val(body_part, DexBuff),
    undefined = val(item, head1).

look_player(_Config) ->
    start(?WORLD_7),
    erlmud_test_socket:send(<<"AnyLoginWillDo">>),
    erlmud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    erlmud_test_socket:send(<<"look pete">>),
    ?WAIT100,
    NakedDescriptions = erlmud_test_socket:messages(),
    ExpectedDescriptions = lists:sort([<<"Pete -> weighs 400.0kg">>,
                                       <<"Pete -> male">>,
                                       <<"Pete -> giant">>,
                                       <<"Pete -> 4.0m tall">>,
                                       <<"Pete -> hands">>,
                                       <<"Pete -> legs">>,
                                       <<"Pete -> pants_: pants">>,
                                       <<"Pete -> sword_: sword">>,
                                       <<"Pete -> scroll_: scroll">>]),
    ExpectedDescriptions = lists:sort(NakedDescriptions).

look_player_clothed(Config) ->
    start(?WORLD_7),
    erlmud_test_socket:send(<<"AnyLoginWillDo">>),
    erlmud_test_socket:send(<<"AnyPasswordWillDo">>),
    Giant = erlmud_index:get(giant),
    attempt(Config, Giant, {move, <<"pants">>, from, Giant, to, <<"legs">>}),
    ?WAIT100,
    erlmud_test_socket:send(<<"look pete">>),
    ?WAIT100,
    ClothedDescriptions = erlmud_test_socket:messages(),
    ct:pal("ClothedDescriptions: ~p", [ClothedDescriptions]),
    ExpectedDescriptions =
        lists:sort([<<"Pete -> hands">>,
                    <<"Pete -> legs">>,
                    <<"Pete -> legs -> pants_: pants">>,
                    <<"Pete -> sword_: sword">>,
                    <<"Pete -> scroll_: scroll">>,
                    <<"Pete -> giant">>,
                    <<"Pete -> weighs 400.0kg">>,
                    <<"Pete -> 4.0m tall">>,
                    <<"Pete -> male">>]),



    ExpectedDescriptions = lists:sort(ClothedDescriptions).

look_room(_Config) ->
    start(?WORLD_7),
    erlmud_test_socket:send(<<"AnyLoginWillDo">>),
    erlmud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    erlmud_test_socket:send(<<"look">>),
    ?WAIT100,
    Descriptions = lists:sort(erlmud_test_socket:messages()),
    ct:pal("Descriptions: ~p~n", [Descriptions]),
    Expected = lists:sort([<<"room -> Bob -> human">>,
                           <<"room -> Pete -> giant">>,
                           <<"room -> bread_: a loaf of bread">>,
                           <<"room: an empty space">>]),
    Descriptions = Expected.

look_item(_Config) ->
    start(?WORLD_7),
    erlmud_test_socket:send(<<"AnyLoginWillDo">>),
    erlmud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    erlmud_test_socket:send(<<"look bread">>),
    ?WAIT100,
    Descriptions = lists:sort(erlmud_test_socket:messages()),
    ct:pal("Descriptions: ~p~n", [Descriptions]),
    Expected = lists:sort([<<"bread_: a loaf of bread">>]),
    Expected = Descriptions.

set_character(Config) ->
    start(?WORLD_9),
    Room = erlmud_index:get(room),
    Dog = erlmud_index:get(dog),
    Collar = erlmud_index:get(collar),
    attempt(Config, Dog, {move, Collar, from, Room, to, Dog}),
    ?WAIT100,
    Dog = val(character, collar),
    Dog = val(character, transmitter),
    Dog = val(character, stealth).

start(Objects) ->
    IdPids = [{Id, start_obj(Id, Props)} || {Id, Props} <- Objects],
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids],
    timer:sleep(100).

start_obj(Id, Props) ->
    {ok, Pid} = supervisor:start_child(erlmud_object_sup, [Id, Props]),
    Pid.

attempt(Config, Target, Message) ->
    TestObject = proplists:get_value(test_object, Config),
    ct:pal("Test object pid: ~p~n", [TestObject]),
    TestObject ! {attempt, Target, Message}.

mock_object() ->
    ct:pal("mock_object ~p receiving~n", [self()]),
    receive
        X ->
            ct:pal("mock_object ~p received: ~p~n", [self(), X]),
            case X of
                {'$gen_call', Msg = {From, MonitorRef}, props} ->
                    ct:pal("mock_object ~p rec'd gen_call: ~p ~n", [self(), Msg]),
                    From ! {MonitorRef, _MockProps = []};
                {attempt, Target, Message} ->
                    ct:pal("mock_object ~p Sending {attempt, ~p, ~p}", [self(), Target, Message]),
                    erlmud_object:attempt(Target, Message, false);
                stop ->
                    ct:pal("mock_object ~p stopping", [self()]),
                    exit(normal);
                Other ->
                    ct:pal("mock_object ~p received other:~n\t~p~n", [self(), Other]),
                    ok
            end
    end,
    mock_object().
