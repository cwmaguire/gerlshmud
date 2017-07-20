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

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

% ???
% Room isn't going to move and it isn't going to have itself as its own
% owner. I suspect this was copy-pasta from the player inject-self handler.
%attempt({_Owner, Props, {Self, move, Direction}}) ->
    %case proplists:get_value(owner, Props) of
        %undefined ->
            %{{fail, <<"Character doesn't have room">>}, false, Props};
        %Room ->
            %{{resend, Self, {Self, move, Room, Direction}}, false, Props}
    %end;
attempt({_Owner, Props, {Self, drop, Pid}}) when Self == self(), is_pid(Pid) ->
    case erlmud_object:has_pid(Props, Pid) of
        true ->
            {owner, Room} = lists:keyfind(owner, 1, Props),
            {{resend, Self, {Self, drop, Pid, Room}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt({_Owner, Props, {Obj, get, Pid}}) when is_pid(Pid) ->
    case erlmud_object:has_pid(Props, Pid) of
        true ->
            log([Obj, <<" resending {get, ">>, Obj, <<", ">>, Pid, <<"} as {get, ">>, Obj, <<", ">>, Pid, <<", ">>, self(), <<"}">>]),
            {{resend, Obj, {get, Obj, Pid, self()}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
