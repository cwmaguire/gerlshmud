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
-module(erlmud_handler_item_inv).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% Track the current owner. When the 'add' succeeds the current owner can remove
%% it from its properties.
attempt({Owner, Props, {move, Self, from, Owner, to, Target, item_body_parts}})
  when Self == self(),
       Owner /= Target,
       is_pid(Target) ->
    BodyParts = proplists:get_value(body_parts, Props, []),
    NewMessage = {move, Self, from, Owner, to, Target, BodyParts},
    Result = {resend, Owner, NewMessage},
    {Result, _Subscribe = true, Props};
attempt({Owner, Props, {move, Self, from, Owner, to, Target}})
  when Self == self(),
       Owner /= Target,
       is_pid(Target) ->
    {succeed, true, Props};
attempt({Owner, Props, {move, Self, from, Owner, to, Target, _ItemBodyParts}})
  when Self == self(),
       Owner /= Target,
       is_pid(Target) ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {move, Self, from, _OldOwner, to, NewOwner}})
  when Self == self() ->
    lists:keystore(owner, 1, Props, {owner, NewOwner});
succeed({Props, {move, Self, from, _OldOwner, to, NewOwner, _ItemBodyParts}})
  when Self == self() ->
    lists:keystore(owner, 1, Props, {owner, NewOwner});

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
