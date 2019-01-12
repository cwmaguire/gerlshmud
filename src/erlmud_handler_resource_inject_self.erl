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
-module(erlmud_handler_resource_inject_self).
-behaviour(erlmud_handler).

%% @doc
%% Captures reservation events reserving this process' character's resource
%% by name and then resends it with this processes pid instead.

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% If something reserves this resource type for this character then we
%% need to inject ourself
attempt({#parents{owner = Owner},
         Props,
         {Owner, reserve, Amt, 'of', ResourceType, for, AttackVector}})
  when is_atom(ResourceType) ->
    Log = [{type, reserve},
           {source, Owner},
           {amount, Amt}],
    case proplists:get_value(type, Props) of
        ResourceType ->
            NewMessage = {Owner, reserve, Amt, 'of', self(), for, AttackVector},

            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = false, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt({#parents{owner = Owner},
         Props,
         {Owner, unreserve, ResourceType, for, AttackVector}}) when is_atom(ResourceType) ->
    Log = [{resource_type, ResourceType},
           {attack_vector, AttackVector}],
    case proplists:get_value(type, Props) of
        ResourceType ->
            NewMessage = {Owner, unreserve, self(), for, AttackVector},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props, Log};
        _ ->
            {succeed, false, Props, Log}
    end;
attempt(_) ->
    undefined.

succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.
