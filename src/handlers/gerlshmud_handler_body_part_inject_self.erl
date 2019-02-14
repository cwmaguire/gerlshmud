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
-module(gerlshmud_handler_body_part_inject_self).
-behaviour(gerlshmud_handler).

-include("include/gerlshmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, Owner, to, BodyPartName}})
  when is_binary(BodyPartName) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?SOURCE, Owner}],
    case is_match(Props, BodyPartName) of
        true ->
            NewMessage = {Item, move, from, Owner, to, self()},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props, [{?TARGET, self()} | Log]};
        _ ->
            {succeed, _Subscribe = false, Props, [{?TARGET, BodyPartName} | Log]}
    end;
attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, BodyPartName, to, Owner}})
  when is_pid(Item) andalso
       is_binary(BodyPartName) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?TARGET, Owner}],
    case is_match(Props, BodyPartName) of
        true ->
            Log2 = [{?SOURCE, self()} | Log],
            NewMessage = {Item, move, from, self(), to, Owner},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props, Log2};
        _ ->
            Log2 = [{?SOURCE, BodyPartName} | Log],
            {succeed, _Subscribe = false, Props, Log2}
    end;
attempt({#parents{owner = Owner},
         Props,
         {Item, move, from, current_body_part, to, Owner}}) ->
    Log = [{item, Item},
           {?EVENT, move},
           {?TARGET, Owner}],
    case [Item_ || {item, {Item_, _Ref}} <- Props, Item_ == Item] of
        [_ | _] ->
            Log2 = [{?SOURCE, self()} | Log],
            NewMessage = {Item, move, from, self(), to, Owner},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props, Log2};
        _ ->
            Log2 = [{?SOURCE, current_body_part} | Log],
            {succeed, _Subscribe = false, Props, Log2}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_match(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, <<>>), Name, [{capture, none}]).
