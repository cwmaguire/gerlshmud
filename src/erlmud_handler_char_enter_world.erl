%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com> %%
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
-module(erlmud_handler_char_enter_world).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Parents,
         Props,
         {Self, enter_world, in, room, with, Conn}}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            log([{type, char_enter_world}, {result, did_not_find_room}]),
            {succeed, false, Props};
        Room ->
            log([{type, char_enter_world}, {result, found_room}]),
            NewMessage = {Self, enter_world, in, Room, with, Conn},
            {{resend, Self, NewMessage}, true, Props}
    end;
attempt({_Parents,
         Props,
         {Self, enter_world, in, Room, with, _Conn}}) when Self == self(), is_pid(Room) ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {Player, enter_world, in, Room, with, Conn}}) ->
    log([{type, char_enter_world},
         {player, Player},
         {result, succeed},
         {room, Room}]),
    lists:foldl(fun keyreplace/2, Props, [{conn_object, Conn}]);
succeed({Props, _Other}) ->
    Props.

fail({Props, Reason, {_Player, enter_world}}) ->
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    Props;
fail({Props, _Reason, _Message}) ->
    Props.

keyreplace(NewKV = {Key, _}, Props) ->
    [NewKV | lists:keydelete(Key, 1, Props)].

log(IoData) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
