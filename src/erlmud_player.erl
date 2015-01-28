-module(erlmud_player).

-export([procs/1]).
-export([create/1]).
-export([handle/2]).

-define(FIELDS, [{room, undefined}, {items, []}, {messages, []}]).
-define(PV(K, PL, Dflt), proplists:get_value(K, PL, Dflt)).
-define(PV(K, PL), ?PV(K, PL, undefined)).

procs(Props) ->
    Fields = [room, items],
    lists:flatten([?PV(Field, Props, Dflt) || {Field, Dflt} <- Fields]).

create(Props) ->
    [{Field, ?PV(Field, Props, [])} || Field <- ?FIELDS].

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
