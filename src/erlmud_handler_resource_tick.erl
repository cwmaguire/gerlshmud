%% Copyright (c) 2017, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_handler_resource_tick).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Owner, Props, {Self, tick, Ref, with, _Count}})
  when Self == self() ->
    case proplists:get_value(tick, Props, undefined) of
        Ref ->
            {succeed, true, Props};
        _ ->
            {fail, unmatched_tick}
    end;

attempt(_) ->
    undefined.

succeed({Props, {Self, tick, Ref, with, Count}})
  when Self == self() ->
    log(debug, [Self, <<" tick">>, <<"\n">>]),
    Current = proplists:get_value(current, Props, 0),
    Max = proplists:get_value(max, Props, 0),
    New = min(Count + Current, Max),
    Reservations = proplists:get_value(reservations, Props, []),
    {RotatedReservations, Remaining} =
        case {Reservations, New} of
            {[], Max} ->
                {[], Max};
            _ ->
                %% For now just make each tick take at _least_ a
                %% second instead of trying to wait close to a second,
                %% or tyring to correct for a long previous tick.
                erlmud_object:attempt_after(1000, Self, {Self, tick, Ref, with, 1}),
                Type = proplists:get_value(type, Props),
                allocate(Type, Reservations, New)
        end,
    OtherProps = proptlists:delete(reservations, proplists:delete(current, Props)),
    [{current, Remaining}, {reservations, RotatedReservations} | OtherProps];

succeed({Props, {_Owner, unreserve, Self, for, Proc}})
  when Self == self() ->
    log(debug, [<<"Unreserving ">>, Self, <<" for ">>, Proc, <<"\n">>]),
    % If we send this to ourself then we can't handle it until after the
    % new reservation property is set
    erlmud_object:attempt(Self, update_tick),
    Reservations = proplists:get(reservations, Props, []),
    [{reservations, lists:delete(Proc, Reservations)} | proplists:delete(reservations, Props)];

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).

allocate(Type, [{Proc, Required} | Reservations], Available)
  when Available > Required ->

    %% Why wait another second? We've already waited a second
    %% for the resource to accumulate.
    %erlmud_object:attempt_after(1000, Proc, {allocate, Required, 'of', Type, to, Proc}),

    erlmud_object:attempt(Proc, {allocate, Required, 'of', Type, to, Proc}),
    RotatedReservations = Reservations ++ [{Proc, Required}],
    allocate(Type, RotatedReservations, Available - Required);
allocate(_, Reservations, Available) ->
    {Reservations, Available}.


