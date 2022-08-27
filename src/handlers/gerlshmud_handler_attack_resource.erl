%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_attack_resource).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

%% respond to resources being added to the owner by reserving
%% those resources and kicking off attacks
%%
%% 1) subscribe to resource increase
%% 2) on resource increase success kick off resource reservation
%% 3) on resource reservation success allocate resources
%% 4) if any attack has all the necessary resources then kick off attack
%% 5) on stop_attack unreserve resources

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({#parents{character = Character,
                  owner = Owner},
         Props,
         {Character, attack, Target, with, Owner}}) ->
    Log = [{?EVENT, attack},
           {?TARGET, Target}],
    {succeed, true, Props, Log};

attempt({#parents{character = Character},
         Props,
         {move, Character, From, To, Exit}}) ->
    Log = [{?EVENT, move},
           {from, From},
           {to, To},
           {exit, Exit}],
    {succeed, true, Props, Log};

attempt({#parents{character = Character},
         Props,
         {Character, die}}) ->
    Log = [{?EVENT, die}],
    {succeed, true, Props, Log};

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, attack, Target, with, Owner}}) ->
    Resources = proplists:get_value(resources, Props, []),
    [reserve(Character, Resource, Amount, Owner) || {Resource, Amount} <- Resources],
    Log = [{?EVENT, attack},
           {?TARGET, Target}],
    {Props, Log};

succeed({Props, {Character, move, From, To, Exit}}) ->
    Owner = proplists:get_value(owner, Props),
    [unreserve(Character, Resource, Owner) || {resource, Resource} <- Props],
    Log = [{?EVENT, move},
           {from, From},
           {to, To},
           {exit, Exit}],
    {Props, Log};

succeed({Props, {Character, die}}) ->
    Owner = proplists:get_value(owner, Props),
    unreserve(Character, Owner, Props),
    Log = [{?EVENT, die}],
    {Props, Log};

succeed({Props, _}) ->
    {Props, _Log = []}.

fail({Props, _, _}) ->
    {Props, _Log = []}.

reserve(Character, Resource, Amount, Owner) ->
    gerlshmud_object:attempt(self(), {Character, reserve, Amount, 'of', Resource, for, Owner}).

unreserve(Character, Owner, Props) when is_list(Props) ->
    [unreserve(Character, Resource, Owner) || {resource, Resource} <- Props];

unreserve(Character, Resource, Owner) ->
    gerlshmud_object:attempt(self(), {Character, unreserve, Resource, for, Owner}).
