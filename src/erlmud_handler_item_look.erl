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

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Owner, Props, {look, Source, TargetName}})
  when Source =/= self(),
       is_binary(TargetName) ->
    log([<<"Checking if name ">>, TargetName, <<" matches">>]),
    SelfName = proplists:get_value(name, Props, <<>>),
    case re:run(SelfName, TargetName, [{capture, none}, caseless]) of
        match ->
            NewMessage = {describe, Source, self()},
            {{resend, Source, NewMessage}, _ShouldSubscribe = true, Props};
        _ ->
            ct:pal("Name ~p did not match this item's name ~p~n", [TargetName, SelfName]),
            log([<<"Name ">>,
                 TargetName,
                 <<" did not match this item's name: ">>,
                 SelfName,
                 <<".\n">>]),
            {succeed, false, Props}
    end;
attempt({_Owner, Props, {describe, _Source, Self}}) when Self == self() ->
    {succeed, true, Props};
attempt({Owner, Props, {describe, _Source, Owner, _Context}}) ->
    {succeed, true, Props};
attempt({Owner, Props, {describe, _Source, Owner, deep, _Context}}) ->
    {succeed, true, Props};
attempt({Owner, Props, {describe, _Source, Owner, _Depth, _Context}}) ->
    {succeed, false, Props};
attempt(_) ->
    undefined.

succeed({Props, {describe, Source, Self}}) when Self == self() ->
    describe(Source, Props),
    Props;
succeed({Props, {describe, Source, Self, Context}}) when Self == self() ->
    describe(Source, Props, Context),
    Props;
succeed({Props, {describe, Source, Target, Context}}) ->
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, Context);
            _ ->
                ok
        end,
    Props;
succeed({Props, {describe, Source, Target, deep, Context}}) ->
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, Context);
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

describe(Source, Props) ->
    Description = description(Props),
    erlmud_object:attempt(Source, {send, Source, [Description]}).

describe(Source, Props, Context) ->
    Description = description(Props),
    erlmud_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

description(Props) when is_list(Props) ->
    DescTemplate = application:get_env(erlmud, item_desc_template, []),
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
