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
-module(erlmud_handler_attack).

%% This generic handler is added to anything that attacks, because
%% each attacking item will control reserving and being allocated
%% resources on its own.

-behaviour(erlmud_handler).

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-define(PROPS, [{hit, 0}, {miss, 0}]).

attempt({Owner, Props, {move, Owner, from, _Src, to, _Target}}) ->
    {succeed, true, Props};
attempt({_Owner, Props, {Attacker, attack, _Target}}) ->
    IsAttackerFun = proplists:get_value(is_attacker_fun, Props),
    case IsAttackerFun(Attacker, Props) of
        true ->
            {succeed, true, Props};
        _ ->
            {succeed, false, Props}
    end;
%% die means that our character has died
attempt({Owner, Props, {die, Owner}}) ->
    {succeed, true, Props};
attempt({_Owner, Props, {_Attacker, calc_hit_on, _Target, by, Self}})
  when Self == self() ->
    {succeed, true, Props};
attempt({_Owner, Props, {_Attacker, calc_damage_to, _Target, by, Self}})
  when Self == self() ->
    {succeed, true, Props};
attempt({_Owner, Props, {_Attacker, does, _Damage, damage_to, Target, with, Self}}) when Self == self(), is_pid(Target) ->
    {succeed, true, Props};
attempt({_Owner, Props, {_Attacker, killed, Target, with, Self}})
  when Self == self(),
       is_pid(Target) ->
    {succeed, true, Props};
attempt({_Owner, Props, {Attacker, stop_attacking, _Target}}) ->
    IsAttackerFun = proplists:get_value(is_attacker_fun, Props),
    case IsAttackerFun(Attacker, Props) of
        true ->
            {succeed, true, Props};
        _ ->
            {succeed, false, Props}
    end;
%attempt({Self, Props, {_Owner, stop, {attack, Self}}}) ->
    %{succeed, _Subscribe = true, Props};

%% I don't think I need this now that there is no central attacking
%% process.
%attempt({_Owner, Props, {gather_body_parts, Self,
                         %_Source, _Target,
                         %_SourceBodyParts, _TargetBodyParts}})
  %when Self == self() ->
    %{succeed, true, Props};
attempt(_) ->
    undefined.

%% We'll get this because we've subscribed to it, because it's our
%% character
succeed({Props, {Source, attack, Target}}) when is_pid(Target) ->
    [reserve(Owner, R) || R <- proplists:get_value(resources, Props, [])],
    lists:keystore(target, 1, Props, {target, Target});
succeed({Props, {_Source, {attack, Self}, UnknownTargetName}})
  when is_list(UnknownTargetName),
       Self == self() ->
    %% Attack failed (no one was self-identified as the target)
    %% TODO: output something to the client like "You swing at imaginary adversaries"
    %%       _if_ this is a player
    Props;
succeed({Props, {Source, calc, Hit, on, Target, with, Self}})
  when is_pid(Target),
       Self == self(),
       Hit > 0 ->
    erlmud_object:attempt(self(), {Source, {attack, self()}, Target, 'calc_damage =', 0}),
    Props;
succeed({Props, {Source, calc, Miss, on, Target, with, Self}})
  when is_pid(Target),
       Self == self() ->
    % TODO: say "you missed!"
    Props;
succeed({Props, {Source, calcs, Damage, damage, to, Target, with, Self}})
  when Self == self(),
       Damage > 0 ->
    erlmud_object:attempt(self(), {Source, does, Damage, damage_to, Target, with, Self}),
    Props;
succeed({Props, {Source, calcs, _NoDamage, damage_to, Target, with, Self}})
  when Self == self() ->
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    Props;
succeed({Props, {die, Owner}}) ->
    %erlmud_object:attempt(self(), {stop, {self(), attack}}),
    [unreserve(Owner, R) || R <- proplists:get_value(resources, Props, [])],
    [{is_attacking, false} | Props];
succeed({Props, {Owner, stop_attack}}) ->
    % We don't want to stop the child process just because
    % the owner has stopped attacking
    %{stop, stop_attack, Props};
    [unreserve(Owner, R) || R <- proplists:get_value(resources, Props, [])],
    [{is_attacking, false} | Props];

succeed({Props, Msg}) ->
    log([<<"saw ">>, Msg, <<" succeed">>]),
    Props.

fail({Props, target_is_dead, _Message}) ->
    log([<<"Stopping because target is dead">>]),
    erlmud_object:attempt(self(), {self(), stop_attack}),
    Props;
fail({Props, _Reason, _Message}) ->
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [?MODULE | Terms]).

unreserve(Owner, Resource) ->
    erlmud_object:attempt(self(), {Owner, reserve, Resource, for, self()}).

unreserve(Owner, Resource) ->
    erlmud_object:attempt(self(), {Owner, unreserve, Resource, for, self()}).

belongs_to_attacker(Attacker, Props) ->
    Attacker == proplists:get_value(top_item, Props).
