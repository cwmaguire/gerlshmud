-module(erlmud_exit).

-export([procs/1]).
-export([create/1]).
-export([handle/2]).

-define(FIELDS, [n, e, s, w, ne, se, nw, sw]).
-define(PV(K, PL, Dflt), proplists:get_value(K, PL, Dflt)).
-define(PV(K, PL), ?PV(K, PL, undefined)).

procs(Props) ->
    Fields = [room, items],
    lists:flatten([?PV(Field, Props, Dflt) || {Field, Dflt} <- Fields]).

create(Props) ->
    Props.

handle(Msg = {attempt, _}, State) ->
    log(Msg, State),
    {true, true, State};
handle(Msg, State) ->
    log(Msg, State),
    State.

log(Msg, State) ->
    io:format("Player received: ~p~n"
              "with state: ~p~n",
              [Msg, State]).
