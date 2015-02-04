-module(erlmud_room).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(Props, {move, Obj, Self, Target}) when Self == self() ->
    io:format("Room ~p: ~p wants to go to ~p~n",
              [self(), Obj, Target]),
    {succeed, true, Props};
attempt(Props, {move, Obj, Source, Self}) when Self == self() ->
    io:format("Room ~p: ~p wants to come from ~p~n", [self(), Obj, Source]),
    {succeed, true, Props};
attempt(Props, Msg) ->
    io:format("Room ~p: ignoring attempt ~p~n", [self(), Msg]),
    {succeed, false, Props}.

succeed(Props, {move, Obj, Self, Target}) when Self == self() ->
    io:format("Room ~p: ~p left for ~p~n", [self(), Obj, Target]),
    Props;
succeed(Props, {move, Obj, Source, Self}) when Self == self() ->
    io:format("Room ~p: ~p came from ~p~n", [self(), Obj, Source]),
    Props;
succeed(Props, {move, Obj, Source, Target}) ->
    io:format("Room ~p: Process ~p went from ~p to ~p~n", [self(), Obj, Source, Target]),
    Props;
succeed(Props, Msg) ->
    io:format("~p saw ~p succeed with props ~p~n", [?MODULE, Msg, Props]),
    Props.

fail(Props, Reason, {move, Obj, Self, Target}) when Self == self() ->
    io:format("Room ~p: ~p couldn't go from here to ~p~n\t~p~n", [self(), Obj, Target, Reason]),
    Props;
fail(Props, Reson, {move, Obj, Source, Self}) when Self == self() ->
    io:format("Room ~p: ~p couldn't come here from ~p~n\t~p~n", [self(), Obj, Source, Reson]),
    Props.
