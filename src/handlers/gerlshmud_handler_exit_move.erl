%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_exit_move).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

%% Exit might not be a cardinal direction like "south", so saying "Direction"
%% doesn't always make sense. e.g. "portal" is not a direction.
attempt({_Owner, Props, {Obj, move, Exit, from, Room}}) when is_atom(Exit) ->
    %% I am an exit process linking two rooms. I have two,
    %% named "exits" pointing to those rooms.
    %% Find an exit that leads _to_ the "FromRoom", that is, it would
    %% go back _into_ the room the object is attempting to leave.
    %% Each exit only leads from one room to another room. We don't want
    %% to try and go from room A through an exit back to A.
    %% So, find the exit that leads to the "FromRoom", which we're trying to
    %% leave, so we don't go back there.
    Log = [{?EVENT, move},
           {?SOURCE, Obj},
           {exit, Exit},
           {from, Room}],
    Rooms = [R || R = {_, Room_} <- Props, Room_ == Room],
    move(Props, Obj, Rooms, Exit, Log);
attempt({_Owner, Props, {Mover, move, from, Source, to, Target, via, Self}}) when Self == self() ->
    Log = [{?EVENT, move},
           {?SOURCE, Mover},
           {from, Source},
           {to, Target},
           {exit, Self}],
    case blocked_reason(Props) of
        {blocked_because, Reason} ->
            {{fail, Reason}, false, Props, Log};
        not_blocked ->
            {succeed, true, Props, Log}
    end;
attempt(_) ->
    undefined.

succeed({Props, Msg}) ->
    Log = [{?SOURCE, self()}, {message, Msg}, {result, succeed}],
    {Props, Log}.

fail({Props, _Result, _Msg}) ->
    Props.

%% Make sure the specified exit name doesn't go back to the room that
%% the object is trying to leave.
%% Consider:
%%
%% ---------          ---------          ---------
%% |       |<"w"------| Obj.  |<"w"------|       |
%% |       |          |       |          |       |
%% |   A   |-- Ex_1 --|   B   |-- Ex_2 --|   C   |
%% |       |          |       |          |       |
%% |       |------"e">|       |------"e">|       |
%% ---------          ---------          ---------
%%
%% An object is in room B.
%% This exit process (i.e. the process that is currently handling this event)
%% could be Ex-1 or Ex-2, since room B is connected to both of
%% these exit processes. If the user is in room B and they say "e", then we
%% need to check both Ex-1 and Ex-2 to see if either of then facilitates
%% going _from_ B to another room via a named exit "e". Ex_1 has an exit
%% named "e", but it goes _to_ B, not _from_ B. Ex_2 also has an exit "e" and
%% it goes _from_ B, ... so this is the exit we want.

%% Below, "FromRoom" is the room we don't want to go back too. The exit that goes there
%% is "FromExit". If we're an exit that has "FromExit" going back to the room
%% an object is trying to leave and it's the same as the "ToExit" they specified
%% then we are not a valid exit for this move. In this case we would be Ex_1 (because
%% we said the object is trying to go "e".)
%% However, if the "FromExit" that goes to "FromRoom" is _different_ than the exit
%% the object is trying to use, then it goes to a different room and we could be
%% a valid exit; i.e. we lead somewhere else with "e".
move(Props, Obj, [{{room, FromExit}, FromRoom}], ToExit, Log) when FromExit /= ToExit ->
    %% This is a bit redundant, but will protect us from maps that are setup
    %% incorrectly: find the opposite exit in our properties that goes the
    %% way the object desires (e.g. "e") and doesn't go back to the room the
    %% object is coming from (this is the redundant part since we check this
    %% in the function "when" guard clause.).
    case [Room || Room = {{room, ToExit_}, ToRoom} <- Props,
                  ToRoom /= FromRoom,
                  ToExit_ == ToExit] of
        %% Now that we've found an exit that doesn't go back to the original room
        %% we can resend the message to specify which room we lead to with exit "ToExit"
        [{_, ToRoom}] ->
            Log2 = [{to, ToRoom},
                    {from_exit, FromExit} | Log],
            NewMsg = {Obj, move, from, FromRoom, to, ToRoom, via, self()},
            {{resend, Obj, NewMsg}, false, Props, Log2};
        [] ->
            {succeed, false, Props, Log}
    end;
move(Props, _, _, _, Log) ->
    {succeed, false, Props, Log}.

%% TODO: add things like is_one_way, is_open, is_right_size, etc.
blocked_reason(Props) ->
    case proplists:get_value(is_locked, Props, false) of
        true ->
            {blocked_because, locked};
        _ ->
            not_blocked
    end.
