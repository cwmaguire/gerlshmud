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
-module(erlmud_handler_char_inject_self).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{}, Props, {Source, Action, TargetName}})
  when is_binary(TargetName) andalso
      (Action == look orelse Action == attack) ->
    Log = [{source, Source},
           {type, Action}],
    case is_name(Props, TargetName) of
        true ->
            Log2 = [{target, self()} | Log],
            NewMessage = {Source, Action, self()},
            Result = {resend, Source, NewMessage},
            {Result, true, Props, Log2};
        _ ->
            Log2 = [{target, TargetName} | Log],
            {succeed, _Subscribe = false, Props, Log2}
    end;
attempt({Owner, Props, {Self, look}}) when Self == self() ->
    Log = [{source, Self},
           {type, look}],
    NewMessage = {Self, look, Owner},
    {{resend, Self, NewMessage}, _ShouldSubscribe = false, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_name(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, ""), Name, [{capture, none}, caseless]).
