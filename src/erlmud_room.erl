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

handle({attempt, {move, Obj, Self, Target}}, State) when Self == self() ->
    io:format("Process ~p wants to leave room ~p for ~p~n",
              [Obj, self(), Target]),
    {true, true, State};
handle({attempt, {move, Obj, Self, Target}}, State) when Self == self() ->
    io:format("Process ~p wants to enter room ~p from ~p~n",
              [Obj, self(), Target]),
    {true, true, State};
handle({succeed, {move, Obj, Self, Target}}, State) when Self == self() ->
    io:format("Process ~p has left room ~p for ~p~n",
              [Obj, self(), Target]),
    State;
handle({succeed, {move, Obj, Source, Self}}, State) when Self == self() ->
    io:format("Process ~p has entered room ~p from ~p~n",
              [Obj, self(), Source]),
    State;
handle({fail, {move, Obj, Self, Target}}, State) when Self == self() ->
    io:format("Process ~p failed to leave this room (~p) for ~p~n",
              [Obj, Self, Target]),
    State;
handle({fail, {move, Obj, Source, Self}}, State) when Self == self() ->
    io:format("Process ~p failed to enter this room (~p) from ~p~n",
              [Obj, Source, Self]),
    State.
