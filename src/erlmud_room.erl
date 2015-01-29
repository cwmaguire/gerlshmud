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
    io:format("~p getting procs~n", [self()]),
    Procs = lists:flatten([?PV(Field, Props) || Field <- Fields]),
    io:format("~p returning procs: ~p~n", [self(), Procs]),
    Procs.

create(Props) ->
    [{Field, ?PV(Field, Props, [])} || Field <- ?FIELDS].

%interested({attempt, move, Obj, Self, Target}) when Self == self() ->
    %io:format("Process ~p wants to leave this room ~p for ~p~n", [Obj, self(), Target]),
    %true;
%interested({attempt, move, Obj, Source, Target}) ->
    %io:format("Process ~p wants to leave room ~p for ~p~n", [Obj, Source, Target]),
    %false.

handle(Props, {attempt, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Room ~p: ~p wants to go to ~p~n",
              [self(), Obj, Target]),
    {succeed, true, Props};
handle(Props, {attempt, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Room ~p: ~p wants to come from ~p~n",
              [self(), Obj, Source]),
    {succeed, true, Props};
handle(Props, {succeed, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Room ~p: ~p left for ~p~n",
              [self(), Obj, Target]),
    Props;
handle(Props, {succeed, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Room ~p: ~p came from ~p~n",
              [self(), Obj, Source]),
    Props;
handle(Props, {succeed, {move, Obj, Source, Target}}) ->
    io:format("Room ~p: Process ~p went from ~p to ~p~n",
              [self(), Obj, Source, Target]),
    Props;
handle(Props, {fail, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Room ~p: ~p couldn't go from here to ~p~n",
              [self(), Obj, Target]),
    Props;
handle(Props, {fail, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Room ~p: ~p couldn't come here from ~p~n",
              [self(), Obj, Source]),
    Props.
