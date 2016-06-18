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
-module(erlmud_handler_exit_move).

-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%id(Props, _Owner, Pid) ->
    %Directions = [atom_to_list(Dir) || {{room, Dir}, _} <- Props],
    %"exit_" ++ string:join(Directions, "_") ++ "_" ++ Pid.

attempt({_Owner, Props, {move, Obj, FromRoom, Exit}}) when is_atom(Exit) ->
    %% If I have an exit to the Source room and a _different_ exit with name Exit
    %% then I should translate the message to a {move, Obj, Source, Target} message.
    log([<<"Process ">>, Obj, <<"wants to leave room ">>, FromRoom, <<" via exit ">>, Exit, <<"\n">>]),
    Rooms = [Room || Room = {_, FromRoom_} <- Props, FromRoom_ == FromRoom],
    move(Props, Obj, Rooms, Exit);
attempt({_Owner, Props, {move, Mover, Source, Target, Self}}) when Self == self() ->
    log([<<"Process ">>, Mover, <<" wants to leave room ">>, Source, <<" for ">>, Target, <<"\n">>]),
    case blocked_reason(Props) of
        {blocked_because, Reason} ->
            {{fail, Reason}, false, Props};
        not_blocked ->
            {succeed, true, Props}
    end;
attempt(_) ->
    undefined.

succeed({Props, Message}) ->
    log([<<"Message ">>, Message, <<" succeeded">>]),
    Props.

fail({Props, Result, Msg}) ->
    log([Result, <<" message: ">>, Msg]),
    Props.

move(Props, Obj, [{{room, FromExit}, FromRoom}], ToExit) when FromExit /= ToExit ->
    case [Room || Room = {{room, ToExit_}, ToRoom} <- Props,
                  ToRoom /= FromRoom,
                  ToExit_ == ToExit] of
        [{_, ToRoom}] ->
            log([<<"Found room ">>, ToRoom, <<" with exit ">>, ToExit, <<" connected to room ">>, FromRoom,
                 <<" (exit ">>, FromExit, <<")">>]),
            NewMsg = {move, Obj, FromRoom, ToRoom, self()},
            {{resend, Obj, NewMsg}, false, Props};
        [] ->
            {succeed, false, Props}
    end;
move(Props, _, _, _) ->
    {succeed, false, Props}.

%% TODO: add things like is_one_way, is_open, is_right_size, etc.
blocked_reason(Props) ->
    case proplists:get_value(is_locked, Props, false) of
        true ->
            {blocked_because, locked};
        _ ->
            not_blocked
    end.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
