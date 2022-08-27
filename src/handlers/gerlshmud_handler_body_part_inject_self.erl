%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_body_part_inject_self).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, Owner, to, BodyPartName}})
  when is_binary(BodyPartName) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Owner}],
    case is_match(Props, BodyPartName) of
        true ->
            NewMessage = {Item, move, from, Owner, to, self()},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props, [{?TARGET, self()} | Log]};
        _ ->
            {succeed, _Subscribe = false, Props, [{?TARGET, BodyPartName} | Log]}
    end;
attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, BodyPartName, to, Owner}})
  when is_pid(Item) andalso
       is_binary(BodyPartName) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?TARGET, Owner}],
    case is_match(Props, BodyPartName) of
        true ->
            Log2 = [{?SOURCE, self()} | Log],
            NewMessage = {Item, move, from, self(), to, Owner},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props, Log2};
        _ ->
            Log2 = [{?SOURCE, BodyPartName} | Log],
            {succeed, _Subscribe = false, Props, Log2}
    end;
attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, current_body_part, to, Owner}}) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?TARGET, Owner}],
    case [Item_ || {item, {Item_, _Ref}} <- Props, Item_ == Item] of
        [_ | _] ->
            Log2 = [{?SOURCE, self()} | Log],
            NewMessage = {Item, move, from, self(), to, Owner},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props, Log2};
        _ ->
            Log2 = [{?SOURCE, current_body_part} | Log],
            {succeed, _Subscribe = false, Props, Log2}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_match(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, <<>>), Name, [{capture, none}]).
