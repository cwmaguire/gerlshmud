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

%% This handler is added to attack processes created on the fly

-behaviour(erlmud_handler).

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-define(PROPS, [{hit, 0}, {miss, 0}]).

attempt({Owner, Props, {move, Owner, from, _Src, to, _Target}}) ->
    {succeed, true, Props};
attempt({Owner, Props, {Owner, {attack, NotSelf}, _Target}}) when self() /= NotSelf ->
    {succeed, true, Props};
%% die means that our character has died
attempt({Owner, Props, {die, Owner}}) ->
    {succeed, true, Props};
attempt({_Owner, Props, {calc_hit, _, {attack, Self}, _, _}}) when Self == self() ->
    case proplists:get_value(done, Props) of
        true ->
            {fail, "It's dead Jim"};
        _ ->
            {succeed, true, Props}
    end;
attempt({Owner, Props, {Owner, {attack, Self}, _Target, with, _Weapon}})
  when Self == self() ->
    {succeed, true, Props};
attempt({Owner, Props, {Owner, {attack, Self}, _Target, with, _Weapon, calc_damage}})
  when Self == self() ->
    {succeed, true, Props};
attempt({Owner, Props, {Owner, {attack, Self}, Target, does, _Damage, damage}}) when Self == self(), is_pid(Target) ->
    {succeed, true, Props};
attempt({Owner, Props, {Owner, killed, Target, with, {attack, Self}}})
  when Self == self(),
       is_pid(Target) ->
    {succeed, true, Props};
attempt({Self, Props, {_Owner, stop, {attack, Self}, _Target}}) ->
    {succeed, _Subscribe = true, Props};
attempt({Self, Props, {_Owner, stop, {attack, Self}}}) ->
    {succeed, _Subscribe = true, Props};

attempt({_Owner, Props, {gather_body_parts, Self,
                         _Source, _Target,
                         _SourceBodyParts, _TargetBodyParts}})
  when Self == self() ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

%% switched targets, stop this attack
succeed({Props, {Owner, {attack, NotSelf}, _Target, with, _Weapon}}) when NotSelf /= self() ->
    erlmud_object:attempt(self(), {Owner, stop, {attack, self()}}),
    Props;
%TODO figure out if I even need this. I don't think it's tested
%succeed({Props, {move, Owner, Target}}) when Action == move ->
    %erlmud_object:attempt(self(), {stop_attack, self(), Owner, Target}),
    %Props;
succeed({Props, {_Source, {attack, Self}, UnknownTargetName}})
  when is_list(UnknownTargetName),
       Self == self() ->
    %% Attack failed (no one was self-identified as the target)
    %% TODO: output something to the client like "You swing at imaginary adversaries"
    %%       _if_ this is a player
    Props;
succeed({Props, {Source, {attack, Self}, Target}}) when is_pid(Target), Self == self() ->
    erlmud_object:attempt(self(), {Source, {attack, self()}, Target, 'calc_hit =', 0}, _Subscribe = true),
    lists:keystore(target, 1, Props, {target, Target});
succeed({Props, {Source, {attack, Self}, Target, 'calc_hit =', HitRoll}})
  when is_pid(Target),
       Self == self(),
       HitRoll > 0 ->
    erlmud_object:attempt(self(), {Source, {attack, self()}, Target, 'calc_damage =', 0}),
    Props;
succeed({Props, {Source, {attack, Self}, Target, 'calc_hit =', _HitRoll}})
  when is_pid(Target),
       Self == self() ->
    % TODO wait for resources
    attack_again(Source, Target),
    Props;
succeed({Props, {Source, {attack, Self}, Target, 'calc_damage =', Damage}})
  when Self == self(),
       Damage > 0 ->
    erlmud_object:attempt(self(), {Source, {attack, self()}, Target, does, Damage, damage}),
    Props;
succeed({Props, {Source, {attack, Self}, Target, 'calc_damage =', _NoDamage}})
  when Self == self() ->
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    attack_again(Source, Target),
    Props;
succeed({Props, {Source, {attack, Self}, Target, does, _Damage, damage}})
  when Self == self() ->
    %% Attack succeeded
    %% TODO: tell the user
    %% The target will have seen this and subtracted damage from itself
    %% and died if necessary.
    attack_again(Source, Target),
    Props;
succeed({Props, {Source, {attack, Self}, Target, 'calc_wait =', Wait, from, Sent}}) ->
    AttackWaitRemaining = millis_remaining(Sent, now(), Wait),
    erlmud_object:attempt_after(AttackWaitRemaining,
                                self(),
                                {Source, {attack, Self}, Target, 'calc_hit =', 1}),
    Props;
succeed({Props, {Owner, die}}) ->
    erlmud_object:attempt(self(), {Owner, stop, {self(), attack}}),
    Props;
succeed({Props, {_Owner, stop, {attack, Self}}}) when Self == self() ->
    {stop, stop_attack, Props};

succeed({Props, Msg}) ->
    log([<<"saw ">>, Msg, <<" succeed">>]),
    Props.

fail({Props, target_is_dead, _Message}) ->
    log([<<"Stopping because target is dead">>]),
    erlmud_object:attempt(self(), {self(), stop_attack}),
    Props;
fail({Props, _Reason, _Message}) ->
    Props.

attack_again(Source, Target) ->
    Now = os:timestamp(),
    CalcWaitMsg = {Source, {attack, self()}, Target, Now, 'calc_wait = ', 0},
    erlmud_object:attempt(self(),CalcWaitMsg),
    ok.

millis_remaining(Time1, Time2, WaitMilis) ->
  ElapsedMilis = abs(subtract(Time2, from, Time1)),
  case(WaitMilis - ElapsedMilis) of
      X when X < 0 ->
          _MinSendAfterTime = 1;
      X ->
          X
  end.

subtract(T1, from, T2) ->
    milis(T2) - milis(T1).

milis({M,S,U}) ->
    M * 1000 * 1000 * 1000 + S * 1000 + round(U / 1000).

log(Terms) ->
    erlmud_event_log:log(debug, [?MODULE | Terms]).
