-module(erlmud_room).

-export([procs/1]).
-export([create/1]).
-export([add/3]).

-define(PV(K, PL), proplists:get_value(K, PL, [])).

procs(Props) ->
    Fields = [exits, players, items, mobs],
    lists:flatten([?PV(Field, Props) || Field <- Fields]).

create(Props) ->
    [{exits, proplists:get_value(exits, Props, [])},
     {players, proplists:get_value(players, Props)},
     {items, proplists:get_value(items, Props)},
     {mobs, proplists:get_value(mobs, Props)}].

add(State = #state{exits = Exits}, exit, Pid) ->
    State#state{exits = [Pid | Exits]};
add(State = #state{items = Items}, item, Pid) ->
    State#state{items = [Pid | Items]};
add(State = #state{players = Players}, player, Pid) ->
    State#state{players = [Pid | Players]};
add(State = #state{mobs = Mobs}, mob, Pid) ->
    State#state{mobs = [Pid | Mobs]}.
