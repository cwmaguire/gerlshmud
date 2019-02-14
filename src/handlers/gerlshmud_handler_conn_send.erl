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
-module(gerlshmud_handler_conn_send).
-behaviour(gerlshmud_handler).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner}, Props, {send, Owner, Message}}) ->
    Log = [{?EVENT, send},
           {?TARGET, Owner},
           {player_message, Message}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.


succeed({Props, {send, Player, Message}}) ->
    Log = [{?EVENT, send},
           {?TARGET, Player},
           {player_message, Message}],
    {Conn} = proplists:get_value(conn, Props),
    gerlshmud_conn:handle(Conn, {send, Message}),
    {Props, Log};
succeed({Props, _Other}) ->
    Props.

fail({Props, _Reason, _Message}) ->
    Props.
