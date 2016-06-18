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
-module(erlmud_handler_conn_move).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({Player, Props, {move, Player, _TheVoid = undefined, _To, _NoExit = undefined}}) ->
    {succeed, true, Props};
attempt(_) ->
    undefined.


succeed({Props, {move, Player, _TheVoid = undefined, _From, _NoExit = undefined}}) ->
    erlmud_object:add(Player, conn_object, self()),
    log(debug, [<<"Player ">>, Player, <<" successfully entered the world\n">>]),
    Props;
succeed({Props, _Other}) ->
    Props.

fail({Props, Reason, {move, _Player, _From, _To, _Exit}}) ->
    {Conn} = proplists:get_value(conn, Props),
    log(debug, [<<"failed to join starting room.">>]),
    Conn ! {disconnect, Reason},
    Props;
fail({Props, _Reason, _Message}) ->
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).

