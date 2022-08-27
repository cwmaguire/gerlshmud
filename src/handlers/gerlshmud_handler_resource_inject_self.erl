%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_resource_inject_self).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

%% @doc
%% Captures reservation events reserving this process' character's resource
%% by name and then resends it with this processes pid instead.

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% If something reserves this resource type for this character then we
%% need to inject ourself
attempt({#parents{owner = Owner},
         Props,
         {Owner, reserve, Amt, 'of', ResourceType, for, AttackVector}})
  when is_atom(ResourceType) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, reserve},
           {amount, Amt},
           {resource_type, ResourceType},
           {vector, AttackVector}],
    case proplists:get_value(type, Props) of
        ResourceType ->
            NewMessage = {Owner, reserve, Amt, 'of', self(), for, AttackVector},

            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         {Owner, unreserve, ResourceType, for, AttackVector}}) when is_atom(ResourceType) ->
    Log = [{?SOURCE, Owner},
           {?EVENT, unreserver},
           {resource_type, ResourceType},
           {vector, AttackVector}],
    case proplists:get_value(type, Props) of
        ResourceType ->
            NewMessage = {Owner, unreserve, self(), for, AttackVector},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
