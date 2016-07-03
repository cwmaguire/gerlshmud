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
-module(erlmud_handler_set_character).

-behaviour(erlmud_handler).

%% @doc Only if the message has our owner do we set the character and
%% then propagate the message. Otherwise we are not a child of the
%% source process and the message shouldn't go any further, so we fail
%% it. Nothing should subscribe to the message.
%%
%% This should cause a cascade of messages that keep starting at the current
%% child and going out to ..
%%
%% This is total cheese since the attempt -> success/fail is not at _all_
%% designed for this, but I haven't taken the time to figure out a better
%% way because I'm an poor excuse for a developer.

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({Owner, Props, {set_character, Owner, Character}}) ->
    NewMessage = {set_character, self(), Character},
    Props2 = lists:keystore(character, 1, Props, {character, Character}),
    {{broadcast, NewMessage}, false, Props2};
attempt({_, Props, {set_character, _, _}}) ->
    {{fail, not_a_child}, _Subscribe = false, Props};
attempt(_) ->
    undefined.


succeed({Props, _Msg}) ->
    Props.

fail({Props, _, _}) ->
    Props.
