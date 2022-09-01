%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_item_inject_self).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{},
         Props,
         {Object, Action, ItemName}})
  when is_binary(ItemName) andalso
       (Action == get orelse
        Action == drop orelse
        Action == look) ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {Object, Action, self()},
            Log = [{?EVENT, inject_self},
                   {action, Action},
                   {name, ItemName}],
            Result = {resend, Object, NewMessage},
            {Result, _Subscribe = false, Props, Log};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt({_Owner, Props, {ItemName, move, from, Source, to, Target}})
  when is_binary(ItemName) ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {self(), move, from, Source, to, Target},
            Log = [{?EVENT, inject_self},
                   {sub_type, move},
                   {name, ItemName}],
            Result = {resend, self(), NewMessage},
            {Result, false, Props, Log};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_name(Props, Name) ->
    ItemName = proplists:get_value(name, Props, ""),
    match == re:run(ItemName, Name, [{capture, none}]).

%log(Props) ->
    %gerlshmud_event_log:log(debug, [{module, ?MODULE} | Props]).
