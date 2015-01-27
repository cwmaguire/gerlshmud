-module(erlmud_player).

-export([procs/1]).
-export([create/1]).
-export([populate/2]).
-export([add/3]).

-record(state, {items :: [pid()],
                room :: pid()}).

procs(#state{items = Items,
             room = Room}) ->
    [Room | Items].

create(Props) ->
    #state{items = proplists:get_value(items, Props, []),
           room = proplists:get_value(room, Props)}.

populate(#state{items = Items, room = Room}, Objs) ->
    #state{items = populate(item, Items, Objs),
           room = populate(room, Room, Objs)}.

populate(item, Items, Objs) ->
    [Obj || Item <- Items,
            Obj <- [proplists:get_value(Item, Objs, undefined)],
            Obj /= undefined].

add(State = #state{items = Items}, item, Pid) ->
    State#state{items = [Pid | Items]}.
