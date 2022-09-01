%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_char_look).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

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
    Description = gerlshmud_util:describe(character, Props),
    gerlshmud_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.
