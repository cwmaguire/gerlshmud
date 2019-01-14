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
-module(erlmud_handler_char_move).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({#parents{}, Props, {Self, move, Direction}}) when Self == self() ->
    Log = [{source, Self},
           {type, move},
           {direction, Direction}],
    case proplists:get_value(owner, Props) of
        undefined ->
            {{fail, <<"Character doesn't have room">>}, false, Props, Log};
        Room ->
            Log2 = [{from, Room} | Log],
            {{resend, Self, {Self, move, Direction, from, Room}}, false, Props, Log2}
    end;
attempt({#parents{}, Props, {Self, move, Dir, from, From}}) when Self == self() ->
    Log = [{source, Self},
           {type, move},
           {direction, Dir},
           {from, From}],
    {succeed, true, Props, Log};
attempt({#parents{}, Props, {Self, move, from, From, to, To, via, Exit}}) when Self == self() ->
    Log = [{source, Self},
           {type, move},
           {from, From},
           {to, To},
           {exit, Exit}],
    {succeed, true, Props, Log};
attempt(_) ->
    undefined.

succeed({Props, {Self, move, from, Source, to, Target, via, Exit}}) when Self == self() ->
    Log = [{type, move},
           {source, Self},
           {from, Source},
           {to, Target},
           {exit, Exit}],
    NewProps = set(room, Target, set(owner, Target, Props)),
    case proplists:get_value(is_attacking, Props) of
        true ->
            erlmud_object:attempt(self(), {self(), stop_attack});
        _ ->
            ok
    end,
    {NewProps, Log};
succeed({Props, {Self, move, Direction, from, Source}}) when Self == self(), is_atom(Direction) ->
    % erlmud_handler_exit_move should have turned this into:
    % {Self, move, from, Source, to, Target, via, Exit}
    Log = [{type, move},
           {source, Self},
           {direction, Direction},
           {from, Source}],
    % TODO Let the player know they didn't get anywhere: "There is no exit <Direction> here."
    {Props, Log};
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {Type, Obj}).
