-module(erlmud_SUITE).
-compile(export_all).

-include("erlmud_test_worlds.hrl").

-define(WAIT100, receive after 100 -> ok end).

all() -> [look].
%all() ->
    %[player_move,
     %player_move_fail,
     %player_move_exit_locked,
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

init_per_testcase(_, Config) ->
    %application:ensure_all_started(cowboy),
    %timer:sleep(2000),
    %application:start(sasl),
    %timer:sleep(2000),
    {ok, _Started} = application:ensure_all_started(erlmud),
    TestObject = spawn_link(fun mock_object/0),
    ct:pal("mock_object is ~p~n", [TestObject]),
    erlmud_index:put("TestObject", TestObject),
    [{test_object, TestObject} | Config].

end_per_testcase(_, _Config) ->
    ct:pal("~p stopping erlmud~n", [?MODULE]),
    application:stop(erlmud).

val(Key, Obj) ->
    Val = proplists:get_value(Key, get_props(Obj)),
    ct:pal("~p for ~p is ~p~n", [Key, Obj, Val]),
    Val.

all(Key, Obj) ->
    proplists:get_all_values(Key, get_props(Obj)).

has(Val, Obj) ->
    false /= lists:keyfind(Val, 2, get_props(Obj)).

get_props(Obj) when is_atom(Obj) ->
    Pid = erlmud_index:get(Obj),
    Props = get_props(Pid),
    ct:pal("Pid for ~p is ~p, props are ~p~n", [Obj, Pid, Props]),
    Props;
get_props(Pid) ->
    {_, _, Props} = sys:get_state(Pid),
    Props.

player_move(Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomSouth =  erlmud_index:get(room_s),

    RoomNorth = val(room, Player),
    attempt(Config, Player, {move, Player, s}),
    ?WAIT100,
    RoomSouth = val(room, Player).

player_move_fail(Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomNorth = val(room, Player),
    attempt(Config, Player, {move, Player, non_existent_exit}),
    ?WAIT100,
    RoomNorth = val(room, Player).

player_move_exit_locked(Config) ->
    start(?WORLD_1),
    Player = erlmud_index:get(player),
    RoomNorth =  erlmud_index:get(room_nw),
    RoomEast =  erlmud_index:get(room_e),
    ExitEastWest =  erlmud_index:get(exit_ew),
    RoomNorth = val(room, Player),
    attempt(Config, Player, {move, Player, e}),
    ?WAIT100,
    RoomNorth = val(room, Player),
    erlmud_object:set(ExitEastWest, {is_locked, false}),
    attempt(Config, Player, {move, Player, e}),
    ?WAIT100,
    RoomEast = val(room, Player).

player_get_item(Config) ->
    start(?WORLD_2),
    Player = erlmud_index:get(player),
    Item = erlmud_index:get(item),
    attempt(Config, Player, {get, Player, <<"sword">>}),
    ?WAIT100,
    has(Item, player).

player_drop_item(Config) ->
    start(?WORLD_2),
    Player = erlmud_index:get(player),
    attempt(Config, Player, {drop, Player, <<"helmet">>}),
    ?WAIT100,
    [] = all(item, Player).

player_attack(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    attempt(Config, Player, {attack, Player, <<"zombie">>}),
    receive after 1000 -> ok end,
    false = val(is_alive, z_life),
    0 = val(hitpoints, z_hp).

player_attack_wait(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    erlmud_object:set(Player, {attack_wait, 10000}),
    attempt(Config, Player, {attack, Player, <<"zombie">>}),
    ?WAIT100,
    5 = val(hitpoints, z_hp),
    true = val(is_alive, z_life),
    true = is_pid(val(attack, Player)).

one_sided_fight(Config) ->
    start(?WORLD_3),
    Player = erlmud_index:get(player),
    Zombie = erlmud_index:get(zombie),
    attempt(Config, Player, {attack, Player, <<"zombie">>}),
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
    %receive after 1000 -> ok end,
    Behaviour = start_obj(behaviour,
                          erlmud_attack_behaviour,
                          [{owner, Zombie},
                           {attack_wait, 10}]),
    erlmud_object:set(Zombie, {behaviours, [Behaviour]}),
    attempt(Config, Player, {attack, Player, <<"zombie">>}),
    receive after 200 -> ok end,
    true = 1000 > val(hitpoints, p_hp),
    true = val(is_alive, p_life),
    undefined = val(attack, Player),
    0 = val(hitpoints, z_hp),
    false = val(is_alive, z_life),
    undefined = val(attack, Zombie),
    ok.

player_wield(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {add, Player, <<"helmet">>, <<"head">>}),
    ?WAIT100,
    Helmet = val(item, head).

player_wield_missing_body_part(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {add, Player, <<"helmet">>, <<"finger">>}),
    ?WAIT100,
    undefined = val(item, head),
    Helmet = val(item, player),
    attempt(Config, Player, {add, Player, <<"helmet">>, <<"head">>}),
    ?WAIT100,
    Helmet = val(item, head),
    undefined = val(item, player).

player_wield_wrong_body_part(Config) ->
    start(?WORLD_5),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {add, Player, <<"helmet">>, <<"finger">>}),
    ?WAIT100,
    undefined = val(item, head1),
    Helmet = val(item, player),
    attempt(Config, Player, {add, Player, <<"helmet">>, <<"head">>}),
    ?WAIT100,
    Helmet = val(item, head1),
    undefined = val(item, player).

player_wield_body_part_is_full(Config) ->
    start(?WORLD_6),
    Player = erlmud_index:get(player),
    Ring1 = erlmud_index:get(ring1),
    Ring2 = erlmud_index:get(ring2),
    [Ring1, Ring2] = all(item, player),
    [] = all(item, finger1),
    [] = all(item, finger2),
    attempt(Config, Player, {add, Player, <<"ring1">>, <<"finger1">>}),
    ?WAIT100,
    [Ring2] = all(item, player),
    [Ring1] = all(item, finger1),
    [] = all(item, finger2),
    attempt(Config, Player, {add, Player, <<"ring2">>, <<"finger1">>}),
    ?WAIT100,
    [Ring2] = all(item, player),
    [Ring1] = all(item, finger1),
    [] = all(item, finger2),
    attempt(Config, Player, {add, Player, <<"ring2">>}),
    ?WAIT100,
    [] = all(item, player),
    [Ring1] = all(item, finger1),
    [Ring2] = all(item, finger2).

player_remove(Config) ->
    start(?WORLD_4),
    Player = erlmud_index:get(player),
    Helmet = erlmud_index:get(helmet),
    attempt(Config, Player, {add, Player, <<"helmet">>, <<"head">>}),
    ?WAIT100,
    undefined = val(item, player),
    Helmet = val(item, head),
    attempt(Config, Player, {remove, Player, <<"helmet">>, <<"head">>}),
    ?WAIT100,
    Helmet = val(item, player),
    undefined = val(item, head),
    attempt(Config, Player, {add, Player, <<"helmet">>, <<"head">>}),
    ?WAIT100,
    undefined = val(item, player),
    Helmet = val(item, head),
    attempt(Config, Player, {remove, Player, <<"helmet">>}),
    ?WAIT100,
    Helmet = val(item, player),
    undefined = val(item, head).

look(_Config) ->
    application:start(sasl),
    dbg:tracer(),
    dbg:p(all, call),
    %dbg:tpl(erlmud_object, [{'_',[],[{return_trace}, {exception_trace}]}]),
    %dbg:tpl(file, write, [{'_',[],[{return_trace}, {exception_trace}]}]),
    dbg:tpl(erlmud_event_log, handle_cast, [{'_',[],[{return_trace}, {exception_trace}]}]),
    %dbg:tpl(erlmud_event_log, log, [{'_',[],[{return_trace}, {exception_trace}]}]),

    start(?WORLD_7),
    %ok = application:start(erlmud),

    %spawn(fun() -> monitor(process, whereis(erlmud_event_log)), loop() end),

    {ok, _TestSocket} = erlmud_test_socket:start(),
    %ct:pal("~p test socket started: ~p~n", [?MODULE, TestSocket]),
    erlmud_test_socket:send(<<"AnyLoginWillDo">>),
    erlmud_test_socket:send(<<"AnyPasswordWillDo">>),
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    erlmud_test_socket:send(<<"look Pete">>),
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    ?WAIT100,
    Descriptions = erlmud_test_socket:messages(),
    ct:pal("Socket messages:~n\t~p~n", [Descriptions]),
    ?WAIT100,
    ?WAIT100.

loop() ->
    %ct:pal("Monitoring erlmud_event_log~n", []),
    %io:format("Monitoring erlmud_event_log~n", []),
    receive
        {'DOWN', _MonitorRef, _Type, Object, Info} ->
            ct:pal("Monitored process ~p triggered ~p~n", [Object, Info])
            %io:format("Monitored process ~p triggered ~p~n", [Object, Info])
        after 200 ->
            %ct:pal("Monitored process erlmud_event_log hasn't triggered anything~n", []),
            %io:format("Monitored process erlmud_event_log hasn't triggered anything~n", []),
            loop()
    end.

start(Objects) ->
    IdPids = [{Id, start_obj(Id, Type, Props)} || {Type, Id, Props} <- Objects],
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids].

start_obj(Id, Type, Props) ->
    {ok, Pid} = supervisor:start_child(erlmud_object_sup, [Id, Type, Props]),
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
