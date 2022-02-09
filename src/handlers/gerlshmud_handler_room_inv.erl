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
-module(gerlshmud_handler_room_inv).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({_Owner, Props, {Item, move, from, Source, to, Target}})
  when Source == self(); Target == self() ->
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Source},
           {to, Target}],
    {succeed, true, Props, Log};
attempt(_Attempt) ->
    undefined.

succeed({Props, {Item, move, from, Self, to, Target}}) when Self == self() ->
    log([<<"Process ">>, Target, <<" got ">>, Item, <<" from me">>]),
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Self},
           {to, Target}],
    Props2 = lists:keydelete(Item, 2, Props),
    {Props2, Log};
succeed({Props, {Item, move, from, Target, to, Self}}) when Self == self() ->
    log([Item, <<" added to me from ">>, Target]),
    Log = [{?SOURCE, Item},
           {?EVENT, move},
           {from, Target},
           {to, Self}],
    Props2 = [{item, Item} | Props],
    {Props2, Log};
succeed({Props, _}) ->
    {Props, _LogProps = []}.

fail({Props, _, _}) ->
    Props.

log(Terms) ->
    gerlshmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
