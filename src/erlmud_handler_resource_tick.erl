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


% if something reserves us and we have the same owner
attempt({_Owner, Props, {Self, update_tick}})
  when Self == self() ->
    {succeed, true, Props};

attempt({_Owner, Props, {Self, tick, Ref, with, _Count}})
  when Self == self() ->
    case proplists:get_value(tick, Props, undefined) of
        Ref ->
            {succeed, true, Props};
        _ ->
            {fail, unmatched_tick}
    end;

% The whole point of this is to update our resource
% on each tick
attempt({_Owner, Props, {Self, tick, _Ref, with, _Count}})
  when Self == self() ->
    {succeed, true, Props};

attempt(_) ->
    undefined.

succeed({Props, {Self, tick, Ref, with, Count}})
  when Self == self() ->
    log(debug, [Self, <<" tick">>, <<"\n">>]),
    Current = proplists:get_value(current, Props, 0),
    Max = proplists:get_value(max, Props, 0),
    New = min(Count + Current, Max),
    Reservations = proplists:get_value(reservations, Props, []),
    case {Reservations, New} of
        {[], Max} ->
            ok;
        _ ->
            erlmud_object:attempt(Self, {Self, tick, Ref, with, 0})
    end,
    [{current, Current} | proplists:delete(current, Props)];

succeed({Props, {Self, update_tick}})
  when Self == self() ->
    log(debug, [Self, <<" updating tick">>, <<"\n">>]),
    Reservations = proplists:get(reservations, Props, []),
    Tick = proplists:get(tick, Props, undefined),
    case {Reservations, Tick} of
        {[_ | _], undefined} ->
            log(debug, [Self, <<" creating new tick">>, <<"\n">>]),
            Ref = make_ref(),
            erlmud_object:attempt(Self, {Self, tick, Ref, with, 0}),
            [{tick, Ref} | Props];
        _ ->
            log(debug, [Self, <<" deleting tick">>, <<"\n">>]),
            proplists:delete(tick, Props)
    end;

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
