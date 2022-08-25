%% Copyright (c) 2022, Chris Maguire <cwmaguire@protonmail.com>
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
-module(gerlshmud_handler_stop).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

%% @doc stop and re-broadcast

-include("include/gerlshmud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{character = Character}, Props, {stop, Character}}) ->
    Log = [{?SOURCE, Character},
           {?TARGET, self()},
           {room, self()},
           {?EVENT, stop}],
    ct:pal("stop (~p) got attempt {stop, ~p}~n", [self(), Character]),
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, Msg = {stop, Character}}) ->
    Log = [{?SOURCE, Character},
           {?TARGET, self()},
           {room, self()},
           {?EVENT, stop}],
    ct:pal("stop (~p) got succeed for ~p~n", [self(), Msg]),
    case proplists:get_value(drop_on_death, Props, false) of
        true ->
            ct:pal("Not stopping (~p) because drop_on_death is true~n", [self()]),
            {Props, Log};
        _ ->
            {stop, finished, Props, Log}
    end;
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
