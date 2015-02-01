-module(erlmud_item).

-export([handle/2]).

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {room, Obj}).

handle(Props, Msg = {attempt, _}) ->
    log(Msg, Props),
    {succeed, true, Props};
handle(Props, {succeed, {move, Self, Source, Target}}) when Self == self() ->
    io:format("Item ~p: moving from ~p to ~p~n", [self(), Source, Target]),
    gen_server:cast(Source, {remove, item, self()}),
    gen_server:cast(Target, {add, item, self()}),
    set(holder, Target, Props);
handle(Props, Msg) ->
    log(Msg, Props),
    Props.

log(Msg, Props) ->
    io:format("Item ~p received: ~p~n"
              "with props: ~p~n",
              [self(), Msg, Props]).

