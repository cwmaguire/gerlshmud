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
-module(erlmud_handler_effect_create).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").
-include("erlmud_handlers.hrl").

attempt({#parents{owner = Spell,
                  character = Character},
         Props,
         {Character, cast, Spell, at, Target}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, cast},
           {?TARGET, Target},
           {spell, Spell}],
    {succeed, true, Props, Log};

attempt({#parents{owner = undefined},
         Props,
         {Character, cast, Spell, at, Target}}) ->
    Log = [{?SOURCE, Character},
           {?EVENT, cast},
           {?TARGET, Target},
           {spell, Spell}],
    {succeed, true, Props, Log};

attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, cast, Spell, at, Target}}) ->
    Log = [{?EVENT, cast},
           {?SOURCE, Character},
           {?TARGET, Target},
           {handler, ?MODULE},
           {spell, Spell}],
    Props2 = proplists:delete(owner, Props),
    {ok, Pid} = supervisor:start_child(erlmud_object_sup, [undefined, Props2]),
    erlmud_object:attempt(Pid, {Pid, affect, Target}),
    {Props, Log};

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

%log(Props) ->
    %erlmud_event_log:log(debug, [{module, ?MODULE} | Props]).
