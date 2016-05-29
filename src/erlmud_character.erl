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
-module(erlmud_character).

-behaviour(erlmud_object).

%% object behaviour
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

-define(ATTEMPT_HANDLERS, [erlmud_handler_room_inject_self,
                           erlmud_handler_char_inject_self,
                           erlmud_handler_char_look,
                           erlmud_handler_self_subscribe,
                           erlmud_handler_attack_wait]).

-define(SUCCESS_HANDLERS, [erlmud_success_char_move]).

id(Props, _Owner, Pid) ->
    "character_" ++ proplists:get_value(name, Props, "NoName") ++ "_" ++ Pid.

added(_, _) -> ok.
removed(_, _) -> ok.

set(Type, Obj, Props) ->
    lists:keystore(Type, 1, Props, {Type, Obj}).

attempt_runner(_, Response = {response, _}) ->
    Response;
attempt_runner(Module, Message) ->
    Module:attempt(Message).

success_runner(_, Response = {response, _}) ->
    Response;
success_runner(Module, Message) ->
    Module:succeed(Message).

attempt(Owner, Props, Message) ->
    lists:foldl(fun attempt_runner/2, {Owner, Props, Message}, ?ATTEMPT_HANDLERS).

succeed(Props, Message) ->
    lists:foldl(fun success_runner/2, {Props, Message}, ?SUCCESS_HANDLERS).

succeed(Props, {move, Self, Source, Target, _Exit}) when Self == self() ->
    log(debug, [<<"moved from ">>, Source, <<" to ">>, Target, <<"\n">>]),
    log(debug, [<<"setting ">>, Self, <<"'s room to ">>, Target, <<"\n">>]),
    NewProps = set(owner, Target, Props),
    log(debug, [<<" finished moving rooms \n">>]),
    NewProps;
succeed(Props, {move, Self, Source, Direction}) when Self == self(), is_atom(Direction) ->
    log(debug, [<<"succeeded in moving ">>, Direction, <<" from ">>, Source, <<"\n">>]),
    Props;
succeed(Props, {get, Self, Source, Item}) when Self == self() ->
    log(debug, [<<"getting ">>, Item, <<" from ">>, Source, <<"\n\tProps: ">>, Props, <<"\n">>]),
    Props;
%% I don't get this: we delete any current attack if a partially started
%% attack (no attack pid, just source and target) succeeds?
succeed(Props, {attack, Self, Target}) when Self == self() ->
    log(debug, [<<"{attack, self(), ">>,
                case Target of
                    TPid when is_pid(TPid) -> TPid;
                    TBin -> TBin
                end,
                <<"} succeeded. Starting attack process">>]),
    attack(Target, lists:keydelete(attack, 1, Props));
succeed(Props, {stop_attack, AttackPid}) ->
    log(debug, [<<"Character ">>, self(), <<" attack ">>, AttackPid, <<" stopped; remove (if applicable) from props:\n\t">>, Props, <<"\n">>]),
    lists:filter(fun({attack, Pid}) when Pid == AttackPid -> false; (_) -> true end, Props);
succeed(Props, {die, Self}) when Self == self() ->
    lists:keydelete(attack, 1, Props);
succeed(Props, {cleanup, Self}) when Self == self() ->
    %% TODO: kill/disconnect all connected processes
    %% TODO: drop all objects
    {stop, cleanup_succeeded, Props};
succeed(Props, {describe, Source, Self, Context}) when Self == self() ->
    describe(Source, Props, deep, Context),
    Props;
succeed(Props, {describe, Source, Target, Context}) ->
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, shallow, Context);
            _ ->
                ok
        end,
    Props;
succeed(Props, Msg) ->
    log(debug, [<<"saw ">>, Msg, <<" succeed\n">>]),
    Props.

fail(Props, target_is_dead, _Message) ->
    log(debug, [<<"Stopping because target is dead\n">>]),
    {stop, Props};
fail(Props, _Message, _Reason) ->
    Props.

attack(Target, Props) ->
    Args = [_Id = undefined,
            _Type = erlmud_attack,
            _Props = [{owner, self()}, {target, Target}]],
    {ok, Attack} = supervisor:start_child(erlmud_object_sup, Args),
    log(debug, [<<"Attack ">>, Attack, <<" started, sending attempt\n">>]),
    erlmud_object:attempt(Attack, {attack, Attack, self(), Target}),
    [{attack, Attack} | Props].

describe(Source, Props, Depth, Context) ->
    Name = proplists:get_value(name, Props),
    NewContext = <<Context/binary, Name/binary, " -> ">>,
    erlmud_object:attempt(Source, {describe, Source, self(), Depth, NewContext}).

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
