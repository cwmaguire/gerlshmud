%% Copyright (c) 2015, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_handler_set_body_part).

-behaviour(erlmud_handler).

%% @doc Only if the message has our owner do we set the body part and
%% then propagate the message. Otherwise we are not a child of the
%% source process and the message shouldn't go any further, so we fail
%% it. Nothing should subscribe to the message.
%%
%% This should cause a cascade of messages that keep starting at the current
%% child and going out.
%%
%% The body_part lets items and their children (sub-items, attributes,
%% spells, etc.) recognize that that they are wielded or worn as opposed
%% to just in the character's inventory.

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({Owner, Props, {set_body_part, Owner, BodyPart}}) ->
    NewMessage = {set_body_part, self(), BodyPart},
    Props2 = lists:keystore(body_part, 1, Props, {body_part, BodyPart}),
    {{broadcast, NewMessage}, false, Props2};
attempt({_, Props, {set_body_part, _, _}}) ->
    {{fail, not_a_child}, _Subscribe = false, Props};
attempt(_) ->
    undefined.


succeed({Props, _Msg}) ->
    Props.

fail({Props, _, _}) ->
    Props.
