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
-module(erlmud_handler_char_attack).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Owner, Props, {calc_next_attack_wait, Attack, Self, Target, Sent, Wait}})
    when Self == self() ->
    ObjWait = proplists:get_value(attack_wait, Props, 0),
    log(debug, [<<"Object attack wait is ">>, ObjWait, <<"\n">>]),
    {succeed,
     {calc_next_attack_wait, Attack, Self, Target, Sent, Wait + ObjWait},
     false,
     Props};
attempt({_Owner, Props, {attack, Self, _Target}}) when Self == self() ->
    {succeed, true, Props};
attempt({_Owner, Props, {calc_hit, _Attack, Self, _Target, _HitRoll}}) when Self == self() ->
    {succeed, true, Props};
attempt({_Onwer, Props, {die, Self}}) when Self == self() ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {attack, Self, Target}}) when Self == self() ->
    log(debug, [<<"{attack, self(), ">>,
                case Target of
                    TPid when is_pid(TPid) -> TPid;
                    TBin -> TBin
                end,
                <<"} succeeded. Starting attack process">>]),
    attack(Target, lists:keydelete(attack, 1, Props));
succeed({Props, {stop_attack, AttackPid}}) ->
    log(debug, [<<"Character ">>, self(), <<" attack ">>, AttackPid, <<" stopped; remove (if applicable) from props:\n\t">>, Props, <<"\n">>]),
    lists:filter(fun({attack, {Pid, _}})
                       when Pid == AttackPid ->
                         false;
                    (_) ->
                         true
                 end,
                 Props);
succeed({Props, {die, Self}}) when Self == self() ->
    lists:keydelete(attack, 1, lists:keydelete(target, 1, Props));
succeed({Props, _}) ->
    Props.

fail({Props, target_is_dead, {calc_hit, _, _, _, _}}) ->
    Attack = proplists:get_value(attack, Props),
    log(debug, [<<"Remove attack ">>, Attack, <<"because target is dead">>]),
    lists:keydelete(attack, 1, Props);
fail({Props, _, _}) ->
    Props.

attack(Target, Props) ->
    Args = [_Id = undefined,
            _Type = erlmud_attack, %% doesn't matter?
            _Props = [{owner, self()}, {target, Target}, {handlers, [erlmud_handler_attack, erlmud_handler_set_character]}]],
    {ok, Attack} = supervisor:start_child(erlmud_object_sup, Args),
    log(debug, [<<"Attack ">>, Attack, <<" started, sending attempt\n">>]),
    erlmud_object:attempt(Attack, {attack, Attack, self(), Target}),
    [{attack, Attack}, {target, Target} | Props].

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).

