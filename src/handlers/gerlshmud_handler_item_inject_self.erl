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
-module(gerlshmud_handler_item_inject_self).
-behaviour(gerlshmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/gerlshmud.hrl").

attempt({_Owner, Props, {Object, Action, ItemName}})
  when is_binary(ItemName) andalso
       Action == get; Action == drop ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {Object, Action, self()},
            Log = [{?EVENT, inject_self},
                   {action, Action},
                   {name, ItemName}],
            Result = {resend, Object, NewMessage},
            {Result, _Subscribe = false, Props, Log};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt({_Owner, Props, {ItemName, move, from, Source, to, Target}})
  when is_binary(ItemName) ->
    case is_name(Props, ItemName) of
        true ->
            NewMessage = {self(), move, from, Source, to, Target},
            Log = [{?EVENT, inject_self},
                   {sub_type, move},
                   {name, ItemName}],
            Result = {resend, self(), NewMessage},
            {Result, false, Props, Log};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_name(Props, Name) ->
    ItemName = proplists:get_value(name, Props, ""),
    match == re:run(ItemName, Name, [{capture, none}]).

%log(Props) ->
    %gerlshmud_event_log:log(debug, [{module, ?MODULE} | Props]).
