-module(erlmud_SUITE).
-compile(export_all).

-include("erlmud.hrl").
-include("erlmud_handlers.hrl").
-include("erlmud_test_worlds.hrl").

-define(WAIT100, receive after 100 -> ok end).

% TODO test updating a skill when a target is killed with a weapon (or when damage is dealt, or both)

all() -> [look_player].
%all() -> [player_drop_item].
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
     %stop_attack_on_move,
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
    Port = ct:get_config(port),
    application:load(erlmud),
    application:set_env(erlmud, port, Port),
    {ok, _Started} = application:ensure_all_started(erlmud),
    {ok, _Pid} = erlmud_test_socket:start(),
    TestObject = spawn_link(fun mock_object/0),
    erlmud_index:put("TestObject", TestObject),
    [{test_object, TestObject} | Config].

end_per_testcase(_, _Config) ->
    ct:pal("~p stopping erlmud~n", [?MODULE]),
    erlmud_test_socket:stop(),
    application:stop(erlmud).

val(Key, Obj) ->
    Props = case get_props(Obj) of
                undefined ->
                    [];
                Props_ ->
                    Props_
            end,
    proplists:get_value(Key, Props).

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
    %erlmud_dbg:add(erlmud_SUITE, start_obj),
    %erlmud_dbg:add(erlmud_object, populate),
    %erlmud_dbg:add(erlmud_object, handle_cast_),
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomSouth =  erlmud_index:get(room_s),

    RoomNorth = val(owner, Player),
    attempt(Config, Player, {Player, move, s}),
    ?WAIT100,
    RoomSouth = val(owner, Player).

player_move_fail(Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomNorth = val(owner, Player),
    attempt(Config, Player, {Player, move, non_existent_exit}),
    ?WAIT100,
    RoomNorth = val(owner, Player).

player_move_exit_locked(Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomEast =  erlmud_index:get(room_e),
    ExitEastWest =  erlmud_index:get(exit_ew),
    RoomNorth = val(owner, Player),
    attempt(Config, Player, {Player, move, e}),
    ?WAIT100,
    RoomNorth = val(owner, Player),
    erlmud_object:set(ExitEastWest, {is_locked, false}),
    attempt(Config, Player, {Player, move, e}),
    ?WAIT100,
    RoomEast = val(owner, Player).

player_get_item(Config) ->
    start(?WORLD_2),
    Player = erlmud_index:get(player),
    Sword = erlmud_index:get(sword),
    true = has(Sword, room),
    false = has(Sword, player),
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
    [] = all(item, Player),
    true = has(Helmet, room).

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
    %WaitFun = 
        %fun() ->
            %val(character, Rifle)
        %end,
    %true = wait_loop(WaitFun, Player, 30),
    wait_value(Rifle, character, Player, 30),
    %Player = val(character, Rifle),
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
    ct:pal("Waiting for player_resource_wait"),
    wait_value(z_hp, hitpoints, 5, 5),
    5 = val(hitpoints, z_hp),
    true = val(is_alive, z_life),
    true = val(is_attacking, p_fist),
    Zombie = val(target, Fist).

one_sided_fight(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    _Zombie = erlmud_index:get(zombie),
    attempt(Config, Player, {Player, attack, <<"zombie">>}),
    WaitFun =
        fun() ->
            case val(hitpoints, z_hp) of
                ZeroOrLess when is_integer(ZeroOrLess), ZeroOrLess =< 0 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun, true, 30),
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    1000 = val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    false = val(is_alive, z_life).

counterattack_behaviour(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    %erlmud_object:set(Player, {attack_wait, 20}),
    %Zombie = erlmud_index:get(zombie),
    %% Counterattack is now handled by items so we'll limit the attacks
    %% with the amount of stamina available
    Stamina = val(stamina, zombie),
    erlmud_object:set(Stamina, {current, 5}),
    erlmud_object:set(Stamina, {tick_time, 100000}),
    Dexterity = erlmud_index:get(dexterity0),
    erlmud_object:set(Dexterity, {defence_hit_modifier, 0}),
    ?WAIT100,
    attempt(Config, Player, {Player, attack, <<"zombie">>}),

    WaitFun =
        fun() ->
            case val(hitpoints, z_hp) of
                ZeroOrLess when is_integer(ZeroOrLess), ZeroOrLess =< 0 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun, true, 30),

    false = val(is_alive, z_life),
    true = 1000 > val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    ok.

attack_with_modifiers(Config) ->
    erlmud_dbg:add(erlmud_handler_item_attack),
    start(?WORLD_8),
    Room = erlmud_index:get(room1),
    Player = erlmud_index:get(player),
    %% TODO make sure that the giant is counterattacking
    _Giant = erlmud_index:get(giant),
    ?WAIT100,
    attempt(Config, Player, {<<"force field">>, move, from, Room, to, Player}),
    attempt(Config, Player, {<<"shield">>, move, from, Room, to, Player}),
    ?WAIT100,
    attempt(Config, Player, {<<"force field">>, move, from, Player, to, first_available_body_part}),
    attempt(Config, Player, {<<"shield">>, move, from, Player, to, first_available_body_part}),
    ?WAIT100,
    attempt(Config, Player, {Player, attack, <<"pete">>}),
    ?WAIT100,

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
    WaitFun2 = 
        fun() ->
            val('is_alive', g_hp)
        end,
    false = wait_loop(WaitFun2, false, 30),
    ok.

stop_attack_on_move(Config) ->
    start(?WORLD_8),
    Room1 = erlmud_index:get(room1),
    Room2 = erlmud_index:get(room2),
    Player = erlmud_index:get(player),

    % TODO make sure the attack stops when the player leaves
    %  - leave
    %  - check that attack is stopped

    _Giant = erlmud_index:get(giant),

    %% Keep the attack going, but so that we can prove that it happened
    GiantHPAmt = 10000000,
    GiantHP =  erlmud_index:get(g_hp),
    erlmud_object:set(GiantHP, {hitpoints, GiantHPAmt}),

    ?WAIT100,
    attempt(Config, Player, {<<"force field">>, move, from, Room1, to, Player}),
    attempt(Config, Player, {<<"shield">>, move, from, Room1, to, Player}),
    ?WAIT100,
    attempt(Config, Player, {<<"force field">>, move, from, Player, to, first_available_body_part}),
    attempt(Config, Player, {<<"shield">>, move, from, Player, to, first_available_body_part}),
    ?WAIT100,
    attempt(Config, Player, {Player, attack, <<"pete">>}),
    ?WAIT100,

    WaitFun1 =
        fun() ->
            case val(hitpoints, g_hp) of
                LessThanFull when is_integer(LessThanFull),
                                  LessThanFull < GiantHPAmt ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun1, true, 30),

    %% Now that the giant has taken some damage, move the player
    %% and make sure the attack stops.

    attempt(Config, Player, {Player, move, r2}),
    WaitFun2 =
        fun() ->
            case val(room, player) of
                Room2 ->
                    true;
                _ ->
                    false
            end
        end,
    true = wait_loop(WaitFun2, true, 30),

    % Make sure the attack has stopped by checking that there
    % are no reservations for the item

    wait_value(p_stamina, reservations, [], 30),
    %WaitFun3 =
        %fun() ->
            %val(reservations, p_stamina)
        %end,
    %true = wait_loop(WaitFun3, [], 30),

    ok.

wait_value(ObjectId, Key, ExpectedValue, Count) ->
    WaitFun =
        fun() ->
            val(Key, ObjectId)
        end,
    true = wait_loop(WaitFun, ExpectedValue, Count).

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
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    ?WAIT100,
    WaitFun =
        fun() ->
            val(item, player)
        end,
    true = wait_loop(WaitFun, undefined, 30),
    %undefined = val(item, Player),
    {Helmet, _BodyPartRef} = val(item, head1),
    {body_part, Head, head, _BodyPartRef} = val(body_part, Helmet).

player_wield_first_available(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Head = erlmud_index:get(head1),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, first_available_body_part}),
    ?WAIT100,
    undefined = val(item, Player),
    {Helmet, _BodyPartRef} = val(item, head1),
    {body_part, Head, head, _BodyPartRef} = val(body_part, Helmet).

player_wield_missing_body_part(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Head = erlmud_index:get(head1),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"finger">>}),
    ?WAIT100,
    undefined = val(item, head1),
    Helmet = val(item, player),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    {Helmet, _BodyPartRef} = val(item, head1),
    {body_part, Head, head, _BodyPartRef} = val(body_part, Helmet),
    undefined = val(item, player).

player_wield_wrong_body_part(Config) ->
    start(?WORLD_5),
    Player = erlmud_index:get(player),
    Head = erlmud_index:get(head1),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"finger">>}),
    ?WAIT100,
    undefined = val(item, head1),
    Helmet = val(item, player),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    {Helmet, _BodyPartRef1} = val(item, head1),
    {body_part, Head, head, _BodyPartRef2} = val(body_part, Helmet),
    undefined = val(item, player).

player_wield_body_part_is_full(Config) ->
    start(?WORLD_6),
    Player = erlmud_index:get(player),
    Finger1 = erlmud_index:get(finger1),
    Finger2 = erlmud_index:get(finger2),
    Ring1 = erlmud_index:get(ring1),
    Ring2 = erlmud_index:get(ring2),
    AllItems = [_, _] = all(item, player),
    true = lists:member(Ring1, AllItems),
    true = lists:member(Ring2, AllItems),
    [] = all(item, finger1),
    [] = all(item, finger2),
    attempt(Config, Player, {<<"ring1">>, move, from, Player, to, <<"finger1">>}),
    ?WAIT100,
    [Ring2] = all(item, player),
    [{Ring1, _BodyPartRef1}] = all(item, finger1),
    {body_part, Finger1, finger, _BodyPartRef2} = val(body_part, Ring1),
    [] = all(item, finger2),
    attempt(Config, Player, {<<"ring2">>, move, from, Player, to, <<"finger1">>}),
    ?WAIT100,
    [Ring2] = all(item, player),
    [{Ring1, _BodyPartRef3}] = all(item, finger1),
    [] = all(item, finger2),
    attempt(Config, Player, {<<"ring2">>, move, from, Player, to, first_available_body_part}),
    ?WAIT100,
    [] = all(item, player),
    [{Ring1, _BodyPartRef4}] = all(item, finger1),
    [{Ring2, _BodyPartRef5}] = all(item, finger2),
    {body_part, Finger2, finger, _BodyPartRef6} = val(body_part, Ring2).

player_remove(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Head = erlmud_index:get(head1),
    Helmet = erlmud_index:get(helmet),
    DexBuff = erlmud_index:get(dex_buff),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    undefined = val(item, player),
    {Helmet, _Ref} = val(item, head1),
    {body_part, Head, head, _Ref0} = val(body_part, Helmet),
    {body_part, Head, head, _Ref1} = val(body_part, DexBuff),
    attempt(Config, Player, {<<"helmet">>, move, from, <<"head">>, to, Player}),
    ?WAIT100,
    Helmet = val(item, player),
    undefined = val(body_part, Helmet),
    undefined = val(body_part, DexBuff),
    undefined = val(item, head1),
    attempt(Config, Player, {<<"helmet">>, move, from, Player, to, <<"head">>}),
    ?WAIT100,
    undefined = val(item, player),
    {Helmet, _Ref2} = val(item, head1),
    {body_part, Head, head, _Ref3} = val(body_part, Helmet),
    {body_part, Head, head, _Ref4} = val(body_part, DexBuff),
    attempt(Config, Player, {<<"helmet">>, move, from, current_body_part, to, Player}),
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
    ExpectedDescriptions = lists:sort([<<"character Pete">>,
                                       <<"Pete -> 400.0kg">>,
                                       <<"Pete -> gender: male">>,
                                       <<"Pete -> race: giant">>,
                                       <<"Pete -> 4.0m tall">>,
                                       <<"Pete -> body part hands">>,
                                       <<"Pete -> body part legs">>,
                                       <<"Pete -> item pants_: pants">>,
                                       <<"Pete -> item sword_: sword">>,
                                       <<"Pete -> item scroll_: scroll">>]),
    ExpectedDescriptions = lists:sort(NakedDescriptions).

look_player_clothed(Config) ->
    start(?WORLD_7),
    erlmud_test_socket:send(<<"AnyLoginWillDo">>),
    erlmud_test_socket:send(<<"AnyPasswordWillDo">>),
    Giant = erlmud_index:get(giant),
    attempt(Config, Giant, {<<"pants">>, move, from, Giant, to, <<"legs">>}),
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
    Expected = lists:sort([<<"room -> character Bob">>,
                           <<"room -> character Pete">>,
                           <<"room -> item bread_: a loaf of bread">>,
                           <<"room: an empty space">>]),
    Expected = Descriptions.

look_item(_Config) ->
    start(?WORLD_7),
    erlmud_test_socket:send(<<"AnyLoginWillDo">>),
    erlmud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    erlmud_test_socket:send(<<"look bread">>),
    ?WAIT100,
    Descriptions = lists:sort(erlmud_test_socket:messages()),
    ct:pal("Descriptions: ~p~n", [Descriptions]),
    Expected = lists:sort([<<"item bread_: a loaf of bread">>]),
    Expected = Descriptions.

set_character(Config) ->
    start(?WORLD_9),
    Room = erlmud_index:get(room),
    Dog = erlmud_index:get(dog),
    Collar = erlmud_index:get(collar),
    attempt(Config, Dog, {Collar, move, from, Room, to, Dog}),
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
    %ct:pal("Test object pid: ~p~n", [TestObject]),
    TestObject ! {attempt, Target, Message}.

mock_object() ->
    %ct:pal("mock_object ~p receiving~n", [self()]),
    receive
        X ->
            %ct:pal("mock_object ~p received: ~p~n", [self(), X]),
            case X of
                {'$gen_call', _Msg = {From, MonitorRef}, props} ->
                    %ct:pal("mock_object ~p rec'd gen_call: ~p ~n", [self(), Msg]),
                    From ! {MonitorRef, _MockProps = []};
                {attempt, Target, Message} ->
                    %ct:pal("mock_object ~p Sending {attempt, ~p, ~p}", [self(), Target, Message]),
                    erlmud_object:attempt(Target, Message, false);
                stop ->
                    %ct:pal("mock_object ~p stopping", [self()]),
                    exit(normal);
                _Other ->
                    %ct:pal("mock_object ~p received other:~n\t~p~n", [self(), Other]),
                    ok
            end
    end,
    mock_object().
