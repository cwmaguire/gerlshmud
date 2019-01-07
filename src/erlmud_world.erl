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
-module(erlmud_world).

-include("erlmud.hrl").
-include("erlmud_handlers.hrl").
-include("play_world.hrl").

-export([init/0]).
-export([move/1]).
-export([m/1]).
-export([s/0]).
-export([t/0]).

init() ->
    IdPids = [{Id, start(Id, Props)} || {Id, Props} <- ?WORLD],
    _Objs = [erlmud_object:populate(Pid, IdPids) || {_, Pid} <- IdPids],
    IdPids.

start(Id, Props) ->
    {ok, Pid} = supervisor:start_child(erlmud_object_sup, [Id, Props]),
    Pid.

move(IdPids) ->
    Room1 = proplists:get_value(room1, IdPids),
    Room2 = proplists:get_value(room2, IdPids),
    Player1 = proplists:get_value(player1, IdPids),
    Procs = {procs, undefined, [], [], []},
    gen_server:cast(Room1, {attempt, {move, Player1, Room1, Room2}, Procs}).

m(Dir) ->
    Player1 = whereis(player1),
    gen_server:cast(Player1, {attempt, {move, Player1, Dir}, {procs, undefined, [], [], []}}).

s() ->
    [io:format("~p: ~p ~p~n", [X, whereis(X), st(X)]) || X <- [player1, room1, room2, room3, exit1, exit2, item1, item2]].

st(Regname) ->
    sys:get_state(Regname).

t() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, [call]),
    dbg:tpl(erlmud_object, [{'_',[],[{return_trace}]}]).
