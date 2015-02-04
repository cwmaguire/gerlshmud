-module(erlmud_item).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {room, Obj}).

attempt(Props, Msg) ->
    log(Msg, Props),
    {succeed, true, Props}.

succeed(Props, {move, Self, Source, Target}) when Self == self() ->
    io:format("Item ~p: moving from ~p to ~p~n", [self(), Source, Target]),
    gen_server:cast(Source, {remove, item, self()}),
    gen_server:cast(Target, {add, item, self()}),
    set(holder, Target, Props);
succeed(Props, Msg) ->
    io:format("~p saw ~p succeed with props ~p~n", [?MODULE, Msg, Props]),
    Props.

fail(Props, Result, Msg) ->
    io:format("~p message: ~p~n", [Result, Msg]),
    Props.

log(Msg, Props) ->
    io:format("Item ~p received: ~p with props: ~p~n",
              [self(), Msg, Props]).

