%% Copyright (c) 2022, Chris Maguire <cwmaguire@gmail.com>
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
-module(gerlshmud_handler_item_cleanup).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{character = Character},
         Props,
         {Character, cleanup, in, _Room}}) ->
    Log = [{?TARGET, Character},
           {?EVENT, cleanup}],
    {succeed, true, Props, Log};
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, cleanup, in, Room}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, cleanup}],
    Owner = proplists:get_value(owner, Props),
    ShouldNotSubscribe = false,
    gerlshmud_object:attempt(self(), {self(), move, from, Owner, to, Room}, ShouldNotSubscribe),
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
