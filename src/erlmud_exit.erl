%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
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

attempt(Props, {move, Obj, FromRoom, Exit}) when is_atom(Exit) ->
    %% If I have an exit to the Source room and a _different_ exit with name Exit
    %% then I should translate the message to a {move, Obj, Source, Target} message.
    io:format("Process ~p wants to leave room ~p via exit ~p~n", [Obj, FromRoom, Exit]),
    Rooms = [Room || Room = {_, FromRoom_} <- Props, FromRoom_ == FromRoom],
    move(Props, Obj, Rooms, Exit);
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

move(Props, Obj, [{{room, FromExit}, FromRoom}], ToExit) when FromExit /= ToExit ->
    case [Room || Room = {{room, ToExit_}, ToRoom} <- Props,
                  ToRoom /= FromRoom,
                  ToExit_ == ToExit] of
        [{_, ToRoom}] ->
            NewMsg = {move, Obj, FromRoom, ToRoom},
            {{resend, Obj, NewMsg}, false, Props};
        [] ->
            {succeed, false, Props}
    end;
move(Props, _, _, _) ->
    {succeed, false, Props}.
