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

-include("include/erlmud.hrl").

attempt({_Parents,
         Props,
         {Self, enter_world, in, room, with, Conn}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, enter_world},
           {conn, Conn}],
    case proplists:get_value(room, Props) of
        undefined ->
            Log2 = [{?TARGET, undefined} | Log],
            {succeed, false, Props, Log2};
        Room ->
            Log2 = [{?TARGET, Room} | Log],
            NewMessage = {Self, enter_world, in, Room, with, Conn},
            {{resend, Self, NewMessage}, true, Props, Log2}
    end;
attempt({_Parents,
         Props,
         {Self, enter_world, in, Room, with, _Conn}}) when Self == self(), is_pid(Room) ->
    Log = [{?SOURCE, Self},
           {?EVENT, enter_world},
           {?TARGET, Room}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Player, enter_world, in, Room, with, Conn}}) ->
    Log = [{?EVENT, char_enter_world},
           {?SOURCE, Player},
           {?TARGET, Room},
           {conn, Conn}],
    Props2 = lists:foldl(fun keyreplace/2, Props, [{conn_object, Conn}]),
    {Props2, Log};
succeed({Props, _Other}) ->
    Props.

fail({Props, Reason, {Player, enter_world}}) ->
    Log = [{?SOURCE, Player},
           {?EVENT, enter_world}],
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    {Props, Log};
fail({Props, _Reason, _Message}) ->
    Props.

keyreplace(NewKV = {Key, _}, Props) ->
    [NewKV | lists:keydelete(Key, 1, Props)].
