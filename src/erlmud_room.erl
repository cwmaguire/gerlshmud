-module(erlmud_room).

-export([add/3]).
-export([remove/3]).
-export([handle/2]).

add(Type, Props, Obj) ->
    case lists:member({Type, Obj}, Props) of
        false ->
            [{Type, Obj} | Props];
        true ->
            Props
    end.

remove(RemType, Obj, Props) ->
    [Prop || Prop = {Type, Pid} <- Props, Type /= RemType, Pid /= Obj].

handle(Props, {attempt, {move, Obj, Self, Target}}) when Self == self() ->
    io:format("Room ~p: ~p wants to go to ~p~n",
              [self(), Obj, Target]), {succeed, true, Props};
handle(Props, {attempt, {move, Obj, Source, Self}}) when Self == self() ->
    io:format("Room ~p: ~p wants to come from ~p~n", [self(), Obj, Source]),
    {succeed, true, Props};

handle(Props, {attempt, Msg}) ->
    io:format("Room ~p: ignoring attempt ~p~n", [self(), Msg]),
    {succeed, false, Props};

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
