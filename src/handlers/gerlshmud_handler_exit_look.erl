%% Copyright (c) 2019, Chris Maguire <cwmaguire@gmail.com>
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
-module(gerlshmud_handler_exit_look).

-behaviour(gerlshmud_handler).

-include("include/gerlshmud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Source, describe, Room, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Owner},
           {room, Room},
           {context, Context}],
    ShouldSub = has_room(Props, Room),
    {succeed, ShouldSub, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Source, describe, Room, with, Context}}) ->
    Log = [{?SOURCE, Source},
           {?EVENT, describe},
           {?TARGET, Room},
           {room, Room},
           {context, Context}],
    describe(Source, Props, Room, Context),
    {Props, Log};
succeed({Props, _Msg}) ->
    {Props, _Log = []}.

-spec fail({proplist(), any(), tuple()}) -> {proplist(), proplist()}.
fail({Props, _Reason, _Msg}) ->
    {Props, _Log = []}.

has_room(Props, Room) ->
    lists:any(is_exit_room_fun(Room), Props).

is_exit_room_fun(Room) ->
  fun ({{room, _}, Room_}) when Room == Room_ ->
      true;
      (_) ->
      false
  end.

describe(Source, Props, Room, Context) ->
    Exits = other_exits(Props, Room),
    ExitsBin = [atom_to_binary(E, utf8) || E <- Exits],
    ExitsDesc = join(<<",">>, ExitsBin),
    Desc = <<Context/binary,
             "exits ",
             ExitsDesc/binary>>,
    gerlshmud_object:attempt(Source, {send, Source, Desc}).

other_exits(Props, Room) ->
    [Dir || {{room, Dir}, NotRoom} <- Props, Room /= NotRoom].

join(Sep, Bins) ->
    join(Sep, Bins, <<>>).

join(_Sep, [], Joined) ->
    Joined;
join(Sep, [Bin | Bins], <<>>) ->
    join(Sep, Bins, <<Bin/binary>>);
join(Sep, [Bin | Bins], Joined) ->
    join(Sep, Bins, <<Joined/binary, Sep, Bin/binary>>).
