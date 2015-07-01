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
-module(erlmud_attack).

-behaviour(erlmud_object).

%% object behaviour
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

-define(PROPS, [{hit, 0}, {miss, 0}]).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(_Owner, Props, Msg) ->
    attempt(Props, Msg).

%attempt(Props, {attack, Self, Source, Target}) when is_pid(Target) ->
    %{{resend, Self, {drop, Self, Source, Target, ?PROPS}}, true, Props};
attempt(Props, {calc_hit, Self, _, _}) when Self == self() ->
    case proplists:get_value(done, Props) of
        true ->
            %{{resend, Self, {move, Self, Room, Direction}}, false, Props}
            {fail, "It's dead Jim"};
        _ ->
            {succeed, true, Props}
    end;
attempt(Props, {Action, Self, _, Target})
  when Self == self(),
       is_pid(Target) ->
    ShouldSubscribe = lists:member(Action, [attack, calc_hit, calc_damage, damage, killed]),
    {succeed, ShouldSubscribe, Props};
attempt(Props, Msg) ->
    log("Attempt: ~p~n\tProps: ~p~n", [Msg, Props]),
    {succeed, false, Props}.

succeed(Props, {attack, Self, _Source, UnknownTargetName})
  when is_list(UnknownTargetName),
       Self == self() ->
    %% Attack failed (no one was self-identified as the target)
    %% TODO: output something to the client like "You swing at imaginary adversaries"
    %%       _if_ this is a player
    Props;
succeed(Props, {attack, Self, Source, Target}) when is_pid(Target), Self == self() ->
    erlmud_object:attempt(self(), {calc_hit, self(), Source, Target, 1}),
    [{target, Target} | Props];
succeed(Props, {calc_hit, Self, Source, Target, HitScore})
  when is_pid(Target),
       Self == self(),
       HitScore > 0 ->
    erlmud_object:attempt(self(), {calc_damage, self(), Source, Target, 1}),
    Props;
succeed(Props, {calc_hit, Self, Source, Target, _Miss})
  when is_pid(Target),
       Self == self() ->
    attack_again(Source, Target),
    Props;
succeed(Props, {calc_damage, Self, Source, Target, Damage})
  when Self == self(),
       Damage > 0 ->
    erlmud_object:attempt(self(), {damage, self(), Source, Target, Damage}),
    Props;
succeed(Props, {calc_damage, Self, Source, Target, _NoDamage})
  when Self == self() ->
    %% Attack failed (No damage was done)
    %% TODO: output something to the client like
    %% "You manage to hit <target> but fail to do any damage"
    %%       _if_ this is a player
    attack_again(Source, Target),
    Props;
succeed(Props, {damage, Self, Source, Target, _Damage})
  when Self == self() ->
    %% Attack succeeded
    %% TODO: tell the user
    %% The target will have seen this and subtracted damage from itself
    %% and died if necessary.
    attack_again(Source, Target),
    Props;
succeed(Props, {calc_next_attack_wait, Self, Source, Target, Sent, Wait}) ->
    erlmud_object:attempt_after(milis_remaining(Sent, now(), Wait),
                                self(),
                                {calc_hit, Self, Source, Target, 1}),
    Props;
succeed(Props, Msg = {killed, Self, _Source, _Target}) when Self == self() ->
    log("Killed it! Huzzah!~nMessage: ~p~nProps: ~p~n", [Msg, Props]),
    [{done, true} | Props];
succeed(Props, Msg) ->
    log("saw ~p succeed with props ~p~n", [Msg, Props]),
    Props.

fail(Props, _Message, _Reason) ->
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

log(Msg, Format) ->
    erlmud_event_log:log("~p:~n" ++ Msg, [?MODULE | Format]).
