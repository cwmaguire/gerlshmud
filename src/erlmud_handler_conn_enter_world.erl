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
-module(erlmud_handler_conn_enter_world).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Owner, enter_world, in, Room, with, Self}}) when Self == self(), is_pid(Room) ->
    {succeed, _Subscribe = true, Props};
attempt(_) ->
    undefined.

succeed({Props, {Player, enter_world, in, Room, with, Conn}}) ->
    log([{type, enter_world}, {player, self()}, {result, succeed}, {conn, Conn}, {room, Room}]),
    [{owner, Player} | lists:keydelete(owner, 1, Props)];
succeed({Props, _Other}) ->
    Props.

%% TODO test that failing to enter the world disconnects the player
fail({Props, Reason, {_Player, enter_world}}) ->
    Conn = proplists:get_value(conn, Props),
    Conn ! {disconnect, Reason},
    Props;
fail({Props, _Reason, _Message}) ->
    Props.

log(Props) ->
    erlmud_event_log:log(debug, [{module, ?MODULE} | Props]).
