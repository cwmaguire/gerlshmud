%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_player).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/2]).
-export([succeed/2]).
-export([fail/3]).

added(_, _) -> ok.
removed(_, _) -> ok.

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {room, Obj}).

attempt(Props, {move, Self, Direction}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Player doesn't have room"}, false, Props};
        Room ->
            {{resend, Self, {move, Self, Room, Direction}}, false, Props}
    end;
attempt(Props, {enter_world, Self}) when Self == self() ->
    case proplists:get_value(room, Props) of
        undefined ->
            {{fail, "Player doesn't have room"}, false, Props};
        Room when is_pid(Room) ->
            {succeed, true, Props}
    end;
attempt(Props, Msg) ->
    log(Msg, Props),
    {succeed, true, Props}.

succeed(Props, {move, Self, Source, Target})
    when Self == self(), is_pid(Target) ->
    io:format("Player ~p: moved from ~p to ~p~n", [self(), Source, Target]),
    erlmud_object:remove(Source, player, self()),
    erlmud_object:add(Source, player, self()),
    set(room, Target, Props);
succeed(Props, {move, Self, Source, Direction})
    when Self == self(), is_atom(Direction) ->
    io:format("Player ~p succeeded in moving to ~p from ~p~n", [self(), Direction, Source]),
    Props;
succeed(Props, {enter_world, Self})
    when Self == self() ->
    Room = proplists:get_value(room, Props),
    io:format("Player ~p: entering ~p~n", [self(), Room]),
    erlmud_object:add(Room, player, self());
succeed(Props, Msg) ->
    io:format("~p saw ~p succeed with props ~p~n", [?MODULE, Msg, Props]),
    Props.

fail(Props, _Message, _Reason) ->
    Props.

log(Msg, Props) ->
    io:format("Player received: ~p props: ~p~n",
              [Msg, Props]).
