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

-behaviour(erlmud_handler).

%% object behaviour
-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

-define(PROPS, [{hit, 0}, {miss, 0}]).

attempt({Owner, Props, {move, Owner, _Src, _Target}}) ->
    {succeed, true, Props};
attempt({Owner, Props, {attack, NotSelf, Owner, _Target}}) when self() /= NotSelf ->
    {succeed, true, Props};
%% die means that our character has died
attempt({Owner, Props, {die, Owner}}) ->
    {succeed, true, Props};
attempt({_Owner, Props, {calc_hit, Self, _, _, _}}) when Self == self() ->
    case proplists:get_value(done, Props) of
        true ->
            {fail, "It's dead Jim"};
        _ ->
            {succeed, true, Props}
    end;
attempt({Owner, Props, {Action, Self, Owner, Target}})
  when Self == self(),
       is_pid(Target) ->
    ShouldSubscribe = lists:member(Action, [attack, calc_damage, damage, killed]),
    {succeed, ShouldSubscribe, Props};
attempt({Owner, Props, {stop_attack, Owner}}) ->
    {succeed, _Subscribe = true, Props};

attempt({_Owner, Props, {gather_body_parts, Self,
                         _Source, _Target,
                         _SourceBodyParts, _TargetBodyParts}})
  when Self == self() ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {Action, NotSelf, Owner, Target}}) when Action == move orelse
                                             (Action == attack andalso
                                              NotSelf /= self()) ->
  erlmud_object:attempt(self(), {stop_attack, self(), Owner, Target}),
  Props;
succeed({Props, {attack, Self, _Source, UnknownTargetName}})
  when is_list(UnknownTargetName),
       Self == self() ->
    %% Attack failed (no one was self-identified as the target)
    %% TODO: output something to the client like "You swing at imaginary adversaries"
    %%       _if_ this is a player
    Props;
succeed({Props, {attack, Self, Source, Target}}) when is_pid(Target), Self == self() ->
    erlmud_object:attempt(self(), {calc_hit, self(), Source, Target, 0}, _Subscribe = true),
    lists:keystore(target, 1, Props, {target, Target});
succeed({Props, {calc_hit, Self, Source, Target, HitScore}})
  when is_pid(Target),
       Self == self(),
       HitScore > 0 ->
    erlmud_object:attempt(self(), {calc_damage, self(), Source, Target, 0}),
    Props;
succeed({Props, {calc_hit, Self, Source, Target, _Miss}})
  when is_pid(Target),
       Self == self() ->
    attack_again(Source, Target),
    Props;
succeed({Props, {calc_damage, Self, Source, Target, Damage}})
  when Self == self(),
       Damage > 0 ->
    erlmud_object:attempt(self(), {damage, self(), Source, Target, Damage}),
    Props;
succeed({Props, {calc_damage, Self, Source, Target, _NoDamage}})
  when Self == self() ->
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    attack_again(Source, Target),
    Props;
succeed({Props, {damage, Self, Source, Target, _Damage}})
  when Self == self() ->
    %% Attack succeeded
    %% TODO: tell the user
    %% The target will have seen this and subtracted damage from itself
    %% and died if necessary.
    attack_again(Source, Target),
    Props;
succeed({Props, {calc_next_attack_wait, Self, Source, Target, Sent, Wait}}) ->
    erlmud_object:attempt_after(milis_remaining(Sent, now(), Wait),
                                self(),
                                {calc_hit, Self, Source, Target, 1}),
    Props;
succeed({Props, {die, _Owner}}) ->
    erlmud_object:attempt(self(), {stop_attack, self()}),
    Props;
succeed({Props, {stop_attack, Self}}) when Self == self() ->
    {stop, stop_attack, Props};

succeed({Props, Msg}) ->
    log([<<"saw ">>, Msg, <<" succeed">>]),
    Props.

fail({Props, target_is_dead, _Message}) ->
    log([<<"Stopping because target is dead">>]),
    erlmud_object:attempt(self(), {stop_attack, self()}),
    Props;
fail({Props, _Reason, _Message}) ->
    Props.

attack_again(Source, Target) ->
    erlmud_object:attempt(self(),
                          {calc_next_attack_wait,
                           self(),
                           Source,
                           Target,
                           os:timestamp(),
                           0}),
    ok.

milis_remaining(Time1, Time2, WaitMilis) ->
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
