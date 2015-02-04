-module(erlmud_player).

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

attempt(Props, {move, Self, Direction}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Player doesn't have room"}, false, Props};
        Room ->
            {{resend, Self, {move, Self, Room, Direction}}, false, Props}
    end;
attempt(Props, Msg) ->
    log(Msg, Props),
    {succeed, true, Props}.

succeed(Props, {move, Self, Source, Target})
    when Self == self(), is_pid(Target) ->
    io:format("Player ~p: moved from ~p to ~p~n", [self(), Source, Target]),
    gen_server:cast(Source, {remove, player, self()}),
    gen_server:cast(Target, {add, player, self()}),
    set(room, Target, Props);
succeed(Props, {move, Self, Source, Direction})
    when Self == self(), is_atom(Direction) ->
    io:format("Player ~p succeeded in moving to ~p from ~p~n", [self(), Direction, Source]),
    Props;
succeed(Props, Msg) ->
    io:format("~p saw ~p succeed with props ~p~n", [?MODULE, Msg, Props]),
    Props.

fail(Props, _Message, _Reason) ->
    Props.

log(Msg, Props) ->
    io:format("Player received: ~p props: ~p~n",
              [Msg, Props]).
