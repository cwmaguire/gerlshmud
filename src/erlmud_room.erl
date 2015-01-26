-module(erlmud_room).

-export([procs/1]).
-export([create/1]).
-export([add/3]).

-record(state, {exits :: [pid()],
                players :: [pid()],
                items :: [pid()],
                mobs :: [pid()]}).

procs(#state{exits = Exits,
             players = Players,
             items = Items,
             mobs = Mobs}) ->
    [Exits | [Players | [Items | Mobs]]].

create(Props) ->
    Props.

add(State = #state{exits = Exits}, exit, Pid) ->
    State#state{exits = [Pid | Exits]};
add(State = #state{items = Items}, item, Pid) ->
    State#state{items = [Pid | Items]};
add(State = #state{players = Players}, player, Pid) ->
    State#state{players = [Pid | Players]};
add(State = #state{mobs = Mobs}, mob, Pid) ->
    State#state{mobs = [Pid | Mobs]}.
