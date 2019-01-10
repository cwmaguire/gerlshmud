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
-module(erlmud_handler_item_look).
-behaviour(erlmud_handler).

-include("include/erlmud.hrl").

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

% TODO move to item_inject_self?
attempt({#parents{}, Props, Msg = {Source, look, TargetName}})
  when Source =/= self(),
       is_binary(TargetName) ->
    SelfName = proplists:get_value(name, Props, <<>>),
    case re:run(SelfName, TargetName, [{capture, none}, caseless]) of
        match ->
            NewMessage = {Source, look, self()},
            log([{stage, attempt},
                 {type, inject_self},
                 {sub_type, look},
                 {object, self()},
                 {props, Props},
                 {source, Source},
                 {name, TargetName},
                 {sub, ignored},
                 {message, Msg},
                 {resend, NewMessage}]),
            {{resend, Source, NewMessage}, _ShouldSubscribe = ignored, Props};
        _ ->
            io:format(user, "Name ~p did not match this item's name ~p~n", [TargetName, SelfName]),
            log([<<"Name ">>,
                 TargetName,
                 <<" did not match this item's name: ">>,
                 SelfName,
                 <<".\n">>]),
            {succeed, false, Props}
    end;
attempt({#parents{}, Props, Msg = {Source, look, Self}}) when Self == self() ->
    log([{stage, attempt},
         {type, look},
         {object, self()},
         {props, Props},
         {source, Source},
         {name, TargetName},
         {sub, true},
         {message, Msg}]),
    {succeed, true, Props};
attempt({#parents{},
         Props,
         {Source, describe, Self, with, Context}}) when Self == self() ->
    log([{stage, attempt},
         {type, describe},
         {object, Self},
         {props, Props},
         {source, Source},
         {target, Self},
         {context, Context},
         {sub, true},
         {message, Msg}]),
    {succeed, true, Props};
attempt({#parents{owner = Owner},
         Props,
         {Source, describe, Owner, with, Context}}) ->
    log([{stage, attempt},
         {type, describe},
         {object, Self},
         {props, Props},
         {source, Source},
         {target, Owner},
         {context, Context},
         {sub, true},
         {message, Msg}]),
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, Msg = {Source, look, Self}}) when Self == self() ->
    log([{stage, succeed},
         {type, look},
         {object, Self},
         {props, Props},
         {source, Source},
         {target, Self},
         {message, Msg}]),
    describe(Source, Props, <<>>, deep),
    Props;
succeed({Props, Msg = {Source, describe, Target, with, Context}}) ->
    _ = case is_owner(Target, Props) of
            true ->
                log([{stage, succeed},
                     {type, describe},
                     {object, Target},
                     {props, Props},
                     {source, Source},
                     {target, Self},
                     {message, Msg},
                     {context, Context}]),
                describe(Source, Props, Context, shallow);
            _ ->
                ok
        end,
    Props;
succeed({Props, _}) ->
    Props.

fail({Props, _, _}) ->
    Props.

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

describe(Source, Props, Context, deep) ->
    send_description(Source, Props, Context),
    Name = proplists:get_value(name, Props),
    NewContext = <<Context/binary, Name/binary, " -> ">>,
    erlmud_object:attempt(Source, {Source, describe, self(), with, NewContext});
describe(Source, Props, Context, shallow) ->
    send_description(Source, Props, Context).

send_description(Source, Props, Context) ->
    Description = description(Props),
    erlmud_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

description(Props) when is_list(Props) ->
    DescTemplate = erlmud_config:desc_template(item),
    log([<<"item desc template: ">>, DescTemplate]),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    RawText;
description_part(Props, DescProp) ->
    log([<<"body part description_part DescProp: ">>, DescProp, <<" from Props: ">>, Props]),
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
