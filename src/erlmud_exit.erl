-module(erlmud_exit).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

%% internal
-export([is_attached_to_room/2]).

added(_, _) -> ok.
removed(_, _) -> ok.

is_attached_to_room(Props, Room) ->
    HasRoom = fun({{room, _}, R}) ->
                  R == Room;
                 (_) ->
                  false
              end,
    lists:any(HasRoom, Props).

attempt(Props, {move, Obj, Source, Exit}) when is_atom(Exit) ->
    %% If I have an exit to the Source room and a _different_ exit with name Exit
    %% then I should translate the message to a {move, Obj, Source, Target} message.
    io:format("Process ~p wants to leave room ~p in direction ~p~n", [Obj, Source, Exit]),
    Room = [Room || Room = {_, ExitRoom} <- Props, ExitRoom == Source],
    move(Props, Obj, Room, Exit);
attempt(Props, {move, Obj, Source, Target}) ->
    io:format("Process ~p wants to leave room ~p for ~p~n", [Obj, Source, Target]),
    {succeed, true, Props};
attempt(Props, Msg) ->
    io:format("~p ~p: ignoring attempt ~p~n", [?MODULE, self(), Msg]),
    {succeed, false, Props}.

succeed(Props, Message) ->
    io:format("Message ~p succeeded~n", [Message]),
    Props.

fail(Props, Result, Msg) ->
    io:format("~p message: ~p~n", [Result, Msg]),
    Props.

move(Props, Obj, [{{room, FromDir}, FromRoom}], FromExit) ->
    case [Room || Room = {{room, ToDir}, ToRoom} <- Props,
                  ToDir /= FromDir,
                  ToRoom /= FromRoom,
                  ToDir == FromExit] of
        [{_, ToRoom}] ->
            NewMsg = {move, Obj, FromRoom, ToRoom},
            Result = {resend, Obj, NewMsg},
            {Result, false, Props};
        [] ->
            {succeed, false, Props}
    end;
move(Props, _, _, _) ->
    {succeed, false, Props}.
