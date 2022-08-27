%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_char_inject_self).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{}, Props, {Source, Action, TargetName}})
  when is_binary(TargetName) andalso
      (Action == look orelse Action == attack) ->
    ct:pal("Running char inject self handler: ~p ~p ~p", [Source, Action, TargetName]),
    Log = [{?SOURCE, Source},
           {?EVENT, Action}],
    case is_name(Props, TargetName) of
        true ->
            Log2 = [{?TARGET, self()} | Log],
            NewMessage = {Source, Action, self()},
            Result = {resend, Source, NewMessage},
            {Result, true, Props, Log2};
        _ ->
            Name = proplists:get_value(name, Props, ""),
            ct:pal("Character (~p) name ~p does not match target name ~p", [self(), Name, TargetName]),
            Log2 = [{?TARGET, TargetName} | Log],
            {succeed, _Subscribe = false, Props, Log2}
    end;
attempt({Owner, Props, {Self, look}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, look}],
    NewMessage = {Self, look, Owner},
    {{resend, Self, NewMessage}, _ShouldSubscribe = false, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_name(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, ""), Name, [{capture, none}, caseless]).
