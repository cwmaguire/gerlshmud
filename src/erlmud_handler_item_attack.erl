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
-module(erlmud_handler_item_attack).
-behaviour(erlmud_handler).

%% This handler is specific to items and controls whether this item
%% process can participate in an attack or not. The character will
%% kick off a generic attack and then the generic attack handler attached
%% to this item process will further kick off a process-specific attack
%% for this process. This handler will listen to that specific attack
%% for it's process and determine if the properties of this item
%% allow for the item to attack. This prevents us from having logic in
%% the generic handler that is specific to items.
%% The generic handler can work out kicking off the hit roll and damage
%% since other handlers and other processes (e.g. attributes) will modify
%% the hit roll and damage with logic specific to this character, body part,
%% item, etc.

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-include("include/erlmud.hrl").

attempt({#parents{character = Character,
                  body_part = BodyPart},
         Props,
         {Character, attack, _Target, with, Self}})
  when Self == self() ->
    ShouldSucceed = wielded(BodyPart, Props) andalso active(Props),
    {ShouldSucceed, ShouldSucceed, Props};

%% If our top-item is attacking then we want to subscribe to that
attempt({#parents{top_item = TopItem},
         Props,
         {_Character, calc, _Hit, on, _Target, with, TopItem}}) ->

    case is_interested() of
        true ->
            erlmud_attack:update_attack(Attack, source, Props);
        _ ->
            {succeed, false, Props}
    end;
attempt({_, _, _Msg}) ->
    undefined.

succeed({Props, {Character, attack, Target}}) when is_pid(Target) ->
    erlmud_object:attempt(self(), {Character, attack, Target, with, self()}),
    Props;
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_interested() ->


wielded(BodyPart, Props) when is_pid(BodyPart) ->
    WieldingBodyParts = proplists:get_value(wielding_body_parts, Props, []),
    case proplists:get_value(body_part, Props) of
        {_, BodyPartType} ->
            lists:member(BodyPartType, WieldingBodyParts);
        _ ->
            false
    end.

active(Props) ->
    proplists:get_value(active, Props, false).

%log(Level, IoData) ->
    %erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
