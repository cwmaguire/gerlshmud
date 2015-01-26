-module(erlmud_player).

-export([procs/1]).
-export([create/1]).
-export([add/3]).

-record(state, {items :: [pid()],
                room :: pid()}).

procs(#state{items = Items,
             room = Room}) ->
    [Room | Items].

create(Props) ->
    Props.

add(State = #state{items = Items}, item, Pid) ->
    State#state{items = [Pid | Items]}.
