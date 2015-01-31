-module(erlmud_room).

-export([procs/1]).
-export([create/1]).
-export([add/3]).
-export([remove/3]).
-export([handle/2]).

-define(FIELDS, [exit, player, item, mob]).

-define(PV(K, PL, Dflt), proplists:get_value(K, PL, Dflt)).
-define(PV(K, PL), proplists:get_value(K, PL, undefined)).

procs(Props) ->
    Fields = [exit, player, item, mob],
    io:format("~p getting procs~n", [self()]),
    Procs = lists:flatten([?PV(Field, Props) || Field <- Fields]),
    io:format("~p returning procs: ~p~n", [self(), Procs]),
    Procs.

create(Props) ->
    [{Field, ?PV(Field, Props, [])} || Field <- ?FIELDS].

add(Type, Props, Obj) ->
    OldProp = proplists:get_value(Type, Props, []),
    NewProp = {Type, [Obj | OldProp]},
    lists:keystore(Type, 1, Props, NewProp).

remove(Type, Obj, Props) ->
    Objs = proplists:get_value(Type, Props, []),
    lists:keystore(Type, 1, Props, {Type, lists:delete(Obj, Objs)}).

handle(Props, {attempt, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Room ~p: ~p wants to go to ~p~n",
              [self(), Obj, Target]), {succeed, true, Props};
handle(Props, {attempt, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Room ~p: ~p wants to come from ~p~n", [self(), Obj, Source]),
    {succeed, true, Props};
handle(Props, {succeed, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Room ~p: ~p left for ~p~n", [self(), Obj, Target]),
    Props;
handle(Props, {succeed, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Room ~p: ~p came from ~p~n", [self(), Obj, Source]),
    Props;
handle(Props, {succeed, {move, Obj, Source, Target}}) ->
    io:format("Room ~p: Process ~p went from ~p to ~p~n", [self(), Obj, Source, Target]),
    Props;
handle(Props, {fail, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Room ~p: ~p couldn't go from here to ~p~n", [self(), Obj, Target]),
    Props;
handle(Props, {fail, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Room ~p: ~p couldn't come here from ~p~n", [self(), Obj, Source]),
    Props.
