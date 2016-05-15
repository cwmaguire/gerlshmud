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
-module(erlmud_room).

-behaviour(erlmud_object).

%% object behaviour
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).

id(_Props, _Owner, Pid) ->
    "room_" ++ Pid.

has_pid(Props, Pid) ->
    lists:any(fun({_, Pid_}) when Pid == Pid_ -> true; (_) -> false end, Props).

added(_, _) -> ok.
removed(_, _) -> ok.

attempt(_Owner, Props, Msg) ->
    attempt(Props, Msg).

attempt(Props, {move, _Obj, Source, Target, _Exit}) when Source == self(); Target == self() ->
    {succeed, true, Props};
attempt(Props, {get, Obj, Pid}) when is_pid(Pid) ->
    case has_pid(Props, Pid) of
        true ->
            log([Obj, <<" resending {get, ">>, Obj, <<", ">>, Pid, <<"} as {get, ">>, Obj, <<", ">>, Pid, <<", ">>, self(), <<"}">>]),
            {{resend, Obj, {get, Obj, Pid, self()}}, true, Props};
        _ ->
            {succeed, _Interested = false, Props}
    end;
attempt(Props, {add, Self, _Player}) when Self == self() ->
    {succeed, true, Props};
attempt(Props, {look, _Source, Self}) when Self == self() ->
    {succeed, true, Props};
attempt(Props, _Msg) ->
    {succeed, false, Props}.

succeed(Props, {move, Obj, Self, Target, _Exit}) when Self == self() ->
    log([Obj, <<" went to ">>, Target]),
    lists:keydelete(Obj, 2, Props);
succeed(Props, {move, Obj, Source, Self, _Exit}) when Self == self() ->
    log([Obj, <<" came from ">>, Source]),
    [{character, Obj} | Props];
succeed(Props, {move, Obj, Source, Target}) ->
    log([<<"Process ">>, Obj, <<" went from ">>, Source, <<" to ">>, Target]),
    Props;
succeed(Props, {get, Obj, Item, Self}) when Self == self() ->
    log([<<"Process ">>, Obj, <<" got ">>, Item, <<" from me">>]),
    Props;
succeed(Props, {add, Self, Player}) when Self == self() ->
    log([<<"Process ">>, Player, <<" added to me">>]),
    Props;
%% Player looking at room
succeed(Props, {look, Player, Self}) when Self == self() ->
    log([<<"Process ">>, Player, <<" looked at me">>]),
    describe(Player, Props),
    Name = proplists:get_value(name, Props, <<"room with no name">>),
    RoomContext = <<Name/binary, " -> ">>,
    %% Resend as Player looking at this Room with Context
    %% which is a key to objects in this room to describe themselves
    NewMessage = {describe, Player, self(), RoomContext},
    erlmud_object:attempt(Player, NewMessage),
    Props;
succeed(Props, Msg) ->
    log([<<"saw unmatched msg ">>, Msg, <<" succeed">>]),
    Props.

% TODO Not sure if this is used.
fail(Props, Reason, {move, Obj, Self, Target}) when Self == self() ->
    log([Obj, <<" couldn't go from here to ">>, Target, <<" ">>, Reason]),
    Props;
fail(Props, Reason, {move, Obj, Source, Target, Exit}) when Source == self(); Target == self() ->
    log([Obj, <<" couldn't move from ">>,
         <<" room ">>, Source, <<" to room ">>, Target,
         <<" via ">>, Exit,
         <<" because ">>, Reason,
         <<", Props: ">>, Props]),
    Props.

describe(Source, Props) ->
    Description = description(Props),
    erlmud_object:attempt(Source, {send, Source, Description}).

description(Props) when is_list(Props) ->
    DescTemplate = application:get_env(erlmud, room_desc_template, []),
    log([<<"description template: ">>, DescTemplate]),
    Description = [[description_part(Props, Part)] || Part <- DescTemplate],
    log([<<"Description: ">>, Description]),
    Description.

description_part(_, RawText) when is_binary(RawText) ->
    log([<<"description_part with unknown Props and RawText: ">>, RawText]),
    RawText;
description_part(Props, DescProp) ->
    log([<<"description_part with Props: ">>, Props, <<", DescProp: ">>, DescProp]),
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
