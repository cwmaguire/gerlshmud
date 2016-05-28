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
-module(erlmud_handler_self_subscribe).
-behaviour(erlmud_handler).

-export([handle/1]).

handle({_Owner, Props, {move, Self, _, _}}) when Self == self() ->
    {succeed, true, Props};
handle({_Owner, Props, {move, Self, _, _, _}}) when Self == self() ->
    {succeed, true, Props};
handle({_Owner, Props, {attack, Self, _}}) when Self == self() ->
    {succeed, true, Props};
handle({_Owner, Props, {stop_attack, Attack}}) ->
    {succeed, _IsCurrAttack = lists:member({attack, Attack}, Props), Props};
handle({_Owner, Props, {die, Self}}) when Self == self() ->
    {succeed, true, Props};
handle(_) ->
    not_interested.
