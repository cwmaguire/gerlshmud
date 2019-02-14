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
-module(erlmud_handler_room_move).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

attempt({_Owner, Props, {Char, move, from, Source, to, Target, via, Exit}})
  when Source == self(); Target == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    {succeed, true, Props, Log};
attempt({_Owner, Props, {Char, enter_world, in, Self, with, Conn}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, enter_world},
           {room, Self},
           {conn, Conn}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Char, move, from, Self, to, Target, via, Exit}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Self},
           {to, Target},
           {exit, Exit}],
    Props2 = lists:keydelete(Char, 2, Props),
    {Props2, Log};
succeed({Props, {Char, move, from, Source, to, Self, via, Exit}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Self},
           {exit, Exit}],
    erlmud_object:attempt(Self, {Char, look, Self}),
    Props2 = [{character, Char} | Props],
    {Props2, Log};
succeed({Props, {Char, enter_world, in, Self, with, Conn}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, enter_world},
           {room, Self},
           {conn, Conn}],
    erlmud_object:attempt(Self, {Char, look, Self}),
    Props2 = [{character, Char} | Props],
    {Props2, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _Reason, {Char, move, from, Self, to, Target}}) when Self == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Self},
           {to, Target}],
    {Props, Log};
fail({Props, _Reason, {Char, move, from, Source, to, Target, via, Exit}}) when Source == self(); Target == self() ->
    Log = [{?SOURCE, Char},
           {?EVENT, move},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    {Props, Log};
fail({Props, _, _}) ->
    Props.
