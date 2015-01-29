-module(erlmud_exit).

-export([procs/1]).
-export([create/1]).
-export([handle/2]).

-define(FIELDS, [rooms]).
-define(PV(K, PL, Dflt), proplists:get_value(K, PL, Dflt)).
-define(PV(K, PL), ?PV(K, PL, undefined)).

procs(Props) ->
    Fields = [room],
    lists:flatten([?PV(Field, Props, Dflt) || {Field, Dflt} <- Fields]).

create(Props) ->
    Props.

handle(Props, {attempt, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Process ~p wants to leave room ~p for ~p~n",
              [Obj, self(), Target]),
    {succeed, true, Props};
handle(Props, {Result, Msg}) ->
    io:format("~p message: ~p~n", [Result, Msg]),
    Props.
