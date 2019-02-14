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
-module(gerlshmud_handler_char_look).
-behaviour(gerlshmud_handler).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{}, Props, {Source, look, TargetName}})
  when Source =/= self(),
       is_binary(TargetName) ->
    Log = [{?EVENT, look},
           {?SOURCE, Source}],
    SelfName = proplists:get_value(name, Props, <<>>),
    case re:run(SelfName, TargetName, [{capture, none}, caseless]) of
        match ->
            Log2 = [{?TARGET, self()} | Log],
            NewMessage = {Source, look, self()},
            {{resend, Source, NewMessage}, _ShouldSubscribe = ignored, Props, Log2};
        _ ->
            Log2 = [{?TARGET, TargetName} | Log],
            {succeed, false, Props, Log2}
    end;
attempt({#parents{owner = Room},
         Props,
        _JustPlainLook = {SelfSource, look}})
  when SelfSource == self() ->
    Log = [{?SOURCE, SelfSource},
           {?EVENT, look},
           {?TARGET, Room}],
    NewMessage = {SelfSource, look, Room},
    {{resend, SelfSource, NewMessage}, _ShouldSubscribe = ignored, Props, Log};
attempt({#parents{},
         Props,
         {Source, look, Self}}) when Self == self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, look},
           {?TARGET, Self}],
    {succeed, true, Props, Log};
attempt({#parents{owner = OwnerRoom},
         Props,
         _DescFromParent = {Source, describe, OwnerRoom, with, RoomContext}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, OwnerRoom},
           {context, RoomContext}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Source, look, Self}}) when Self == self() ->
    Log = [{?SOURCE, Source},
           {?EVENT, look},
           {?TARGET, Self}],
    NoContext = <<>>,
    describe(Source, Props, NoContext, deep),
    {Props, Log};
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

describe(Source, Props, Context, shallow) ->
    send_description(Source, Props, Context);
describe(Source, Props, Context, deep) ->
    send_description(Source, Props, Context),
    Name = proplists:get_value(name, Props),
    NewContext = <<Context/binary, Name/binary, " -> ">>,
    gerlshmud_object:attempt(Source, {Source, describe, self(), with, NewContext}).

send_description(Source, Props, Context) ->
    Description = description(Props),
    gerlshmud_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

description(Props) when is_list(Props) ->
    DescTemplate = gerlshmud_config:desc_template(character),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    RawText;
description_part(Props, DescProp) ->
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.
