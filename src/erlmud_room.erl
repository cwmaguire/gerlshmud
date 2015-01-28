-module(erlmud_room).

-export([procs/1]).
-export([create/1]).
%-export([interested/1]).
-export([handle/2]).

-define(FIELDS, [exits, players, items, mobs]).

-define(PV(K, PL, Dflt), proplists:get_value(K, PL, Dflt)).
-define(PV(K, PL), proplists:get_value(K, PL, undefined)).

procs(Props) ->
    Fields = [exits, players, items, mobs],
    lists:flatten([?PV(Field, Props) || Field <- Fields]).

create(Props) ->
    [{Field, ?PV(Field, Props, [])} || Field <- ?FIELDS].

%interested({attempt, move, Obj, Self, Target}) when Self == self() ->
    %io:format("Process ~p wants to leave this room ~p for ~p~n", [Obj, self(), Target]),
    %true;
%interested({attempt, move, Obj, Source, Target}) ->
    %io:format("Process ~p wants to leave room ~p for ~p~n", [Obj, Source, Target]),
    %false.

handle({attempt, {move, Obj, Self, Target}}, Props) when Self == self() ->
    io:format("Process ~p wants to leave room ~p for ~p~n",
              [Obj, self(), Target]),
    {succeed, true, Props};
handle({attempt, {move, Obj, Self, Target}}, Props) when Self == self() ->
    io:format("Process ~p wants to enter room ~p from ~p~n",
              [Obj, self(), Target]),
    {succeed, true, Props};
handle(Props, {succeed, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Process ~p has left this room ~p for ~p~n",
              [Obj, self(), Target]),
    Props;
handle(Props, {succeed, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Process ~p has entered this room ~p from ~p~n",
              [Obj, self(), Source]),
    Props;
handle(Props, {succeed, {move, Obj, Source, Target}}) ->
    io:format("Process ~p has entered room ~p from ~p (This process is ~p)~n",
              [Obj, Target, Source, self()]),
    Props;
handle(Props, {fail, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Process ~p failed to leave this room (~p) for ~p~n",
              [Obj, Self, Target]),
    Props;
handle(Props, {fail, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Process ~p failed to enter this room (~p) from ~p~n",
              [Obj, Source, Self]),
    Props.
