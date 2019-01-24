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
-module(erlmud_handler_body_part_look).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{}, Props, {Source, describe, Self, with, Context}}) when Self == self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Self},
           {context, Context}],
    {succeed, true, Props, Log};
attempt({#parents{owner = Owner}, Props, {Source, describe, Owner, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Owner},
           {context, Context}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Source, describe, Self, with, Context}}) when Self == self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Self},
           {context, Context}],
    Props2 = describe(Source, Props, Context, deep),
    {Props2, Log};
succeed({Props, {Source, describe, Target, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Target},
           {context, Context}],
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, Context, shallow);
            _ ->
                ok
        end,
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

describe(Source, Props, Context, deep) ->
    send_description(Source, Props, Context),
    Name = proplists:get_value(name, Props),
    NewContext = <<Context/binary, Name/binary, " -> ">>,
    erlmud_object:attempt(Source, {Source, describe, self(), with, NewContext});
describe(Source, Props, Context, shallow) ->
    send_description(Source, Props, Context).

send_description(Source, Props, Context) ->
    Description = description(Props),
    erlmud_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

description(Props) when is_list(Props) ->
    DescTemplate = erlmud_config:desc_template(body_part),
    log([{?EVENT, body_part_desc}, {template, DescTemplate}]),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    log([{?EVENT, body_part_desc}, {desc_part, RawText}]),
    RawText;
description_part(Props, DescProp) ->
    log([{?EVENT, body_part_desc}, {desc_prop, DescProp}, {props, Props}]),
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

log(IoData) ->
    erlmud_event_log:log(debug, [{module, ?MODULE} | IoData]).
