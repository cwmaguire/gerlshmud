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
-module(erlmud_handler_char_inv).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Owner, Props, {Self, drop, Item}}) when Self == self(), is_pid(Item) ->
    case erlmud_object:has_pid(Props, Item) of
        true ->
            Room = proplists:get_value(owner, Props),
            %{{resend, Self, {Self, drop, Pid, to, Room}}, true, Props};
            {{resend, Self, {move, item, Item, from, Self, to, Room}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt({_Owner, Props, {move, item, _Item, from, Self, to, _Room}}) when Self == self() ->
    {succeed, true, Props};
attempt({_Owner, Props, {move, item, _Item, from, _Room, to, Self}}) when Self == self() ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {Self, remove, Item, from, BodyPart}}) when Self == self() ->
    log(debug, [<<"Removing ">>, Item, <<" from body part ">>, BodyPart, <<" into general inventory.\n\tProps: ">>, Props, <<"\n">>]),
    [{item, Item} | Props];
succeed({Props, {move, item, Item, from, Source, to, Self}}) when Self == self() ->
    log(debug, [<<"Getting ">>, Item, <<" from ">>, Source, <<"\n\tProps: ">>, Props, <<"\n">>]),
    [{item, Item} | Props];
succeed({Props, {move, item, Item, from, Self, to, Target}}) when Self == self() ->
    log(debug, [<<"Moving ">>, Item, <<" to ">>, Target, <<"\n\tProps: ">>, Props, <<"\n">>]),
    lists:keydelete(Item, 2, Props);
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
