%% Copyright (c) 2016, Chris Maguire <cwmaguire@gmail.com>
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
-module(gerlshmud_handler_char_cleanup).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{}, Props, {Self, Cleanup}) when Self == self() ->
    Log = [{?TARGET, Self},
           {?EVENT, cleanup}],
    {succeed, true, Props, Log}.

succeed({Props, {Self, cleanup}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, cleanup}],
    Room = proplists:get_value(owner, Props),
    ShouldSubscribe = true,
    gerlshmud_object:attempt(self(), {self(), cleanup, in, Room}, ShouldSubscribe),
    {Props, Log};

succeed({Props, {Self, cleanup, in, _Room}}) ->
    [unreserve(Character, Resource, Owner) || {resource, Resource} <- Props],
    Log = [{?EVENT, move},
           {from, From},
           {to, To},
           {exit, Exit}],
    gerlshmud_object:attempt(self(), stop),
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
