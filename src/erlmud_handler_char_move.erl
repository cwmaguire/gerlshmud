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

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Owner, Props, {move, Self, _, _}}) when Self == self() ->
    {succeed, true, Props};
attempt({_Owner, Props, {move, Self, _, _, _}}) when Self == self() ->
    {succeed, true, Props};
attempt(_) ->
    not_interested.

succeed({Props, {move, Self, Source, Target, _Exit}}) when Self == self() ->
    log(debug, [<<"moved from ">>, Source, <<" to ">>, Target, <<"\n">>]),
    log(debug, [<<"setting ">>, Self, <<"'s room to ">>, Target, <<"\n">>]),
    NewProps = set(owner, Target, Props),
    log(debug, [<<" finished moving rooms \n">>]),
    NewProps;
succeed({Props, {move, Self, Source, Direction}}) when Self == self(), is_atom(Direction) ->
    log(debug, [<<"succeeded in moving ">>, Direction, <<" from ">>, Source, <<"\n">>]),
    Props;
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {Type, Obj}).

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).

