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
-module(erlmud_handler_spell_attack).
-behaviour(erlmud_handler).

%% This generic handler is added to anything that attacks, because
%% each attacking item will control reserving and being allocated
%% resources on its own. That is, there isn't a single attack process
%% for the character that manages all the different attack vectors.
%% Each attack vector registers for resource allocation and fires off
%% attacks whenever all of its resource needs are fulfilled.

-include("include/erlmud.hrl").

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

%% This process' parent character is attacking a target
attempt({#parents{character = Character}, Props, {Character, attack, Target}}) ->
    Log = [{source, Character},
           {type, attack},
           {target, Target}],
    {succeed, true, Props, Log};

attempt({#parents{character = Character}, Props, {Character, attack, Target, with, Self}})
  when Self == self() ->
    Log = [{source, Character},
           {type, attack},
           {target, Target},
           {vector, Self}],
    {succeed, true, Props, Log};

%% This process landed an attack on a target
attempt({#parents{}, Props, {Character, calc, Hit, on, Target, with, Self}})
  when Self == self() ->
    Log = [{source, Character},
           {type, calc_hit},
           {hit, Hit},
           {target, Target},
           {vector, Self}],
    {succeed, true, Props, Log};

%% This process needs to calculate damage to a target
%% TODO that needs to happen in the attempt
%%
%% I don't think I can do this: how will I know if a separate handler is handling
%% this?
%%
%%    Well, I could only use this for top level items, spells, etc. that don't
%%    need custom logic and then not even add it to sub-items and stuff that
%%    need custom filters (e.g. a sub-item checks if the attack vector matches
%%    its top-item).
%%
%%    One possible way around this is to have a general 'attack_hit_modifier' as
%%    well as a specific property (e.g. 'subitem_attack_hit_modifier') for the
%%    sub-item handler.
%%
%%    Maybe have a way to override handler events?
%%       Cowboy checks the module to see which functions are exported, but
%%       every handler will export attempt/3 and succeed/1.
%%
%%       Perhaps when erlmud_object grabs the handlers from the process?
%%       but ... we'd need to cache the results otherwise we're doing that
%%       on _every_ event.
%%
%%       Or, I could have several generic handlers:
%%          - is_interested
%%          - calc hit
%%          - calc damage
%%       ... but I'd run into trouble if I wanted both to respond to the
%%       success of the same event.
%%
%%    I think I'll stick with a generic handler until I need something specific
%%    and then I'll move that process over completely to custom handlers.
attempt({#parents{}, Props, {Character, calc, Damage, to, Target, with, Self}})
  when Self == self() ->
    Log = [{source, Character},
           {type, calc_damage},
           {damage, Damage},
           {target, Target},
           {vector, Self}],
    {succeed, true, Props, Log};

%% This process did damage to a target
attempt({#parents{}, Props, {Character, does, Damage, to, Target, with, Self}})
  when Self == self(), is_pid(Target) ->
    Log = [{source, Character},
           {type, damage},
           {damage, Damage},
           {target, Target},
           {vector, Self}],
    {succeed, true, Props, Log};

%% All processes belonging to this character need to stop attacking
attempt({#parents{character = Character}, Props, {Character, stop_attacking, Target}}) ->
    Log = [{source, Character},
           {type, stop_attacking},
           {target, Target}],
    {succeed, true, Props, Log};

attempt(_) ->
    undefined.

%% An attack by our character has been successfully instigated but with no
%% specfic attack vector: we'll kick off an attempt to attack with this item
%% specifically.
succeed({Props, {Character, attack, Target}}) when is_pid(Target) ->
    Log = [{source, Character},
           {type, attack},
           {target, Target}],
    erlmud_object:attempt(self(), {Character, attack, Target, with, self()}),
    {Props, Log};

%% An attack by our character has been successfully instigated using this process:
%% we'll register for resources and implement the attack when we have them.
succeed({Props, {Character, attack, Target, with, _Self}}) ->
    reserve(Character, proplists:get_value(resources, Props, [])),
    lists:keystore(target, 1, Props, {target, Target});

succeed({Props, {Character, calc, Hit, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       Hit > 0 ->
    Log = [{source, Character},
           {type, calc_hit},
           {hit, Hit},
           {target, Target},
           {vector, Self}],
    erlmud_object:attempt(self(), {Character, calc, _InitialDamage = 0, to, Target, with, Self}),
    {Props, Log};
succeed({Props, {Character, calc, Miss, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    Log = [{source, Character},
           {type, calc_hit},
           {hit, Miss},
           {target, Target},
           {vector, Self}],
    % TODO: say "you missed!"
    {Props, Log};
succeed({Props, {Character, calc, Damage, to, Target, with, Self}})
  when Self == self(),
       Damage > 0 ->
    Log = [{source, Character},
           {type, calc_damage},
           {damage, Damage},
           {target, Target},
           {vector, Self}],
    erlmud_object:attempt(self(), {Character, does, Damage, to, Target, with, Self}),
    {Props, Log};
succeed({Props, {Character, calc, NoDamage, to, Target, with, Self}})
  when Self == self() ->
    Log = [{source, Character},
           {type, calc_damage},
           {damage, NoDamage},
           {target, Target},
           {vector, Self}],
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    {Props, Log};
succeed({Props, {Character, stop_attack}}) ->
    Log = [{source, Character},
           {type, stop_attack}],
    unreserve(Character, Props),
    Props2 = [{is_attacking, false} | Props],
    {Props2, Log};

succeed({Props, Msg}) ->
    log([<<"saw ">>, Msg, <<" succeed">>]),
    Props.

fail({Props, target_is_dead, _Message}) ->
    Log = [{type, target_is_dead}],
    erlmud_object:attempt(self(), {self(), stop_attack}),
    {Props, Log};
fail({Props, _Reason, _Message}) ->
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [?MODULE | Terms]).

unreserve(Owner, Props) ->
    reserve_op(unreserve, Owner, Props).

reserve(Owner, Props) ->
    reserve_op(reserve, Owner, Props).

reserve_op(Op, Character, Props) when is_list(Props) ->
    [reserve_op(Op, Character, R) || R <- proplists:get_value(resources, Props, [])];

reserve_op(Op, Character, Resource) ->
    erlmud_object:attempt(self(), {Character, Op, Resource, for, self()}).
