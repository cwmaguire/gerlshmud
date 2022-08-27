%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_handler_resource_reserve).
-behaviour(gerlshmud_handler).
-compile({parse_transform, gerlshmud_protocol_parse_transform}).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

% If something reserves us and we have the same owner (character).
attempt({#parents{owner = Owner}, Props, {Owner, reserve, Amount, 'of', Self, for, Proc}})
  when Self == self() ->
    Log = [{?SOURCE, Owner},
           {?EVENT, reserve},
           {amount, Amount},
           {?TARGET, Self},
           {for, Proc}],
    {succeed, true, Props, Log};
attempt({#parents{owner = Owner}, Props, {Owner, unreserve, Self, for, Proc}})
  when Self == self() ->
    Log = [{?SOURCE, Owner},
           {?EVENT, unreserve},
           {?TARGET, Self},
           {for, Proc}],
    {succeed, true, Props, Log};
attempt({#parents{}, Props, {Self, update_tick}}) when Self == self() ->
    Log = [{?SOURCE, Self},
           {?EVENT, update_tick}],
    {succeed, false, Props, Log};

attempt(_) ->
    undefined.

succeed({Props, {Character, reserve, Amount, 'of', Self, for, Proc}})
  when Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, reserve},
           {amount, Amount},
           {?TARGET, Self},
           {handler, ?MODULE},
           {for, Proc}],
    Reservations = proplists:get_value(reservations, Props, []),
    Props2 = case lists:member({Proc, Amount}, Reservations) of
                 true ->
                     Props;
                 false ->
                    [{reservations, Reservations ++ [{Proc, Amount}]} | proplists:delete(reservations, Props)]
             end,
    %io:format("New props (with reservations): ~p~n", [Props2]),
    Props3 = update_tick(Props2),
    {Props3, Log};
succeed({Props, {Character, unreserve, Self, for, Proc}})
  when Self == self() ->
    Log = [{?SOURCE, Character},
           {?EVENT, unreserve},
           {?TARGET, Self},
           {handler, ?MODULE},
           {for, Proc}],
    Reservations = proplists:get_value(reservations, Props, []),
    Props2 = lists:keystore(reservations, 1, Props, {reservations, lists:keydelete(Proc, 1, Reservations)}),
    Props3 = update_tick(Props2),
    {Props3, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

update_tick(Props) ->
    Self = self(),
    Reservations = proplists:get_value(reservations, Props, []),
    Tick = proplists:get_value(tick, Props, undefined),
    PerTick = proplists:get_value(per_tick, Props, 1),
    case {Reservations, Tick} of
        {[_ | _], undefined} ->
            Ref = make_ref(),
            gerlshmud_object:attempt(Self, {Self, tick, Ref, with, PerTick}),
            [{tick, Ref} | Props];
        {[], _} ->
            lists:keydelete(tick, 1, Props);
        _ ->
            Props
    end.
