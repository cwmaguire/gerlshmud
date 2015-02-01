-module(erlmud_exit).

-export([procs/1]).
-export([create/1]).
-export([handle/2]).

-define(FIELDS, [rooms]).
-define(PV(K, PL, Dflt), proplists:get_value(K, PL, Dflt)).
-define(PV(K, PL), ?PV(K, PL, undefined)).

procs(Props) ->
    Rooms = ?PV(rooms, Props, []),
    lists:flatten([Room || {_, Room} <- Rooms]).

create(Props) ->
    Props.

handle(Props, {attempt, {move, Obj, Source, Exit}}) when is_atom(Exit) ->
    %% If I have an exit to the Source room and a _different_ exit with name Exit
    %% then I should translate the message to a {move, Obj, Source, Target} message.
    io:format("Process ~p wants to leave room ~p in direction ~p~n", [Obj, Source, Exit]),
    Room = [Room || Room = {_, ExitRoom} <- Props, ExitRoom == Source],
    move(Props, Obj, Room, Exit);
handle(Props, {attempt, {move, Obj, Source, Target}}) ->
    io:format("Process ~p wants to leave room ~p for ~p~n", [Obj, Source, Target]),
    {succeed, true, Props};
handle(Props, {Result, Msg}) ->
    io:format("~p message: ~p~n", [Result, Msg]),
    {fail, false, Props}.

move(Props, Obj, [{{room, FromDir}, FromRoom}], FromExit) ->
    case [Room || Room = {{room, ToDir}, ToRoom} <- Props,
                  ToDir /= FromDir,
                  ToRoom /= FromRoom,
                  ToDir == FromExit] of
        [{_, ToRoom}] ->
            {move, Obj, FromRoom, ToRoom};
        [] ->
            {succeed, false, Props}
    end.
