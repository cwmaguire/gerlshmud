%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_handler_attribute_look).

-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {describe, _Source, Owner, deep, _Context}}) ->
    {succeed, true, Props};
attempt({#parents{owner = Owner},
         Props,
         {describe, _Source, Owner, shallow, _Context}}) ->
    ShouldSubscribe = _AttributeIsRace = race == proplists:get_value(type, Props),
    {succeed, ShouldSubscribe, Props};
attempt(_) ->
    undefined.

succeed({Props, {describe, Source, _Owner, _Depth, Context}}) ->
    describe(Source, Props, Context),
    Props;
succeed({Props, Msg}) ->
    log([<<"saw unmatched msg ">>, Msg, <<" succeed">>]),
    Props.

fail({Props, _Reason, _Msg}) ->
    Props.

describe(Source, Props, Context) ->
    DescTemplate = proplists:get_value(desc, Props, []),
    Value = proplists:get_value(value, Props),
    Desc = [case X of value -> Value; X -> X end || X <- DescTemplate],
    erlmud_object:attempt(Source, {send, Source, [Context | Desc]}).

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
