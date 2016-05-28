%% Copyright (c) 2016, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_handler_room_inject_self).
-behaviour(erlmud_handler).

-export([handle/1]).

handle({_Owner, Props, {move, Self, Direction}}) ->
    case proplists:get_value(owner, Props) of
        undefined ->
            {{fail, <<"Character doesn't have room">>}, false, Props};
        Room ->
            {{resend, Self, {move, Self, Room, Direction}}, false, Props}
    end;
handle({_Owner, Props, {drop, Self, Pid}}) when Self == self(), is_pid(Pid) ->
    case erlmud_object:has_pid(Props, Pid) of
        true ->
            {owner, Room} = lists:keyfind(owner, 1, Props),
            {{resend, Self, {drop, Self, Pid, Room}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
handle(_) ->
    not_interested.

