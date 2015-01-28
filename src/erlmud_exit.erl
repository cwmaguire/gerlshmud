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

handle(Props, Msg = {attempt, _}) ->
    log(Msg, Props),
    {succeed, true, Props};
handle(Msg, Props) ->
    log(Msg, Props),
    Props.

log(Msg, Props) ->
    io:format("Player received: ~p~n"
              "with props: ~p~n",
              [Msg, Props]).
