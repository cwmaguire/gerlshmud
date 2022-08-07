%% Copyright (c) 2021, Chris Maguire <cwmaguire@protonmail.com>
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
-module(gerlshmud_handler_delete_property).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-include("include/gerlshmud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{},
         Props,
         {delete, Pid}}) ->
    Log = [{?SOURCE, Pid},
           {?EVENT, delete}],
    Props2 = lists:keydelete(Pid, 2, Props),
    {succeed, false, Props2, Log};
attempt(_) ->
    undefined.

succeed({Props, _Msg}) ->
    Props.

fail({Props, _, _}) ->
    Props.
