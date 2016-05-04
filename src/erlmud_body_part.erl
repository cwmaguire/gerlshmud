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
-module(erlmud_body_part).

-behaviour(erlmud_object).

%% object behaviour
-export([id/3]).
-export([added/2]).
-export([removed/2]).
-export([attempt/3]).
-export([succeed/2]).
-export([fail/3]).
-export([died/3]).

id(Props, Owner, Pid) ->
    Name = proplists:get_value(name, Props, "NoName"),
    "body_part_" ++ Name ++ "_of_" ++ Owner ++ "_" ++ Pid.

added(_, _) -> ok.
removed(_, _) -> ok.
died(_, _, _) -> ok.

is_match(Props, Name) ->
    match == re:run(proplists:get_value(name, Props, <<>>), Name, [{capture, none}]).

can(add, Props, Item) ->
    can_add(Props, Item);
can(remove, Props, Item) ->
    can_remove(Props, Item).

can_add(Props, Item) ->
    can_add([fun has_matching_body_part/2,
             fun has_space/2], Props, Item, true).

can_add([], _, _, Result) ->
    log([<<"can_add([], _, _, ">>, Result ,<<")">>]),
    Result;
can_add(_, _, _, {false, Reason}) ->
    log([<<"can_add([_ | _], _, _, {false, ">>, Reason,<<"})">>]),
    {false, Reason};
can_add([Fun | Funs], Props, Item, true) ->
    log([<<"can_add([">>, Fun, <<" | ">>, Funs, <<"], ">>, Props, <<", ">>, Item, <<", true)">>]),
    can_add(Funs, Props, Item, Fun(Props, Item)).

has_matching_body_part(Props, Item) ->
    BodyPart = proplists:get_value(body_part, Props, any),
    %% TODO Remove synchronous gen_server call: have the item send it's
    %% body parts when it sees it's name and replaces it with it's PID.
    ItemBodyParts = lists:flatten(erlmud_object:get(Item, body_parts)),
    log([<<"has_matching_body_part(">>, BodyPart,
         ", ", ItemBodyParts, "):",
         " {", BodyPart,
         lists:member(BodyPart, ItemBodyParts), "}"]),
    case {BodyPart, lists:member(BodyPart, ItemBodyParts)} of
        {any, _} ->
            true;
        {_, true} ->
            true;
        {_, _} ->
            {false, "Item is not compatible with body part"}
    end.

has_space(Props, _) ->
    NumItems = length(proplists:get_all_values(item, Props)),
    MaxItems = proplists:get_value(max_items, Props, infinite),
    log([<<"has_space(">>, Props, <<") Num items: ">>, NumItems, <<" Max items: ">>, MaxItems]),
    case proplists:get_value(max_items, Props, infinite) of
        infinite ->
            true;
        MaxItems when NumItems < MaxItems ->
            true;
        _ ->
            {false, "Body part is full"}
    end.

can_remove(_Props, _Item) ->
    true.

has_owner(Item, Owner) when is_pid(Item) ->
    [Owner] == erlmud_object:get(Item, owner).

attempt(Owner, Props, {Action, Owner, Item, BodyPartName})
  when is_pid(Item) andalso
       is_binary(BodyPartName) andalso
       (Action == add orelse
        Action == remove) ->
    case is_match(Props, BodyPartName) of
        true ->
            NewMessage = {Action, Item, self()},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(_Owner, Props, {Action, Item, Self})
  when Self == self() andalso
       is_pid(Item) andalso
       (Action == add orelse
        Action == remove) ->
    case can(Action, Props, Item) of
        {false, Reason} ->
            {{fail, Reason}, _Subscribe = false, Props};
        _ ->
            {succeed, _Subscribe = true, Props}
    end;
attempt(Owner, Props, {add, Owner, Item}) ->
    case is_pid(Item) andalso
         has_owner(Item, Owner) andalso
         can_add(Props, Item) of
        true ->
            NewMessage = {add, Item, self()},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(Owner, Props, {remove, Owner, Item}) ->
    case {item, Item} == lists:keyfind(Item, 2, Props) of
        true ->
            NewMessage = {remove, Item, self()},
            Result = {resend, Owner, NewMessage},
            {Result, _Subscribe = true, Props};
        _ ->
            {succeed, _Subscribe = false, Props}
    end;
attempt(Owner, Props, {describe, _Source, Owner, _Context, _SubDescs}) ->
    {succeed, true, Props};
attempt(_Owner, Props, {describe, _Source, _Target, _Context, _SubDescs}) ->
    {succeed, false, Props};
attempt(_Owner, Props, _Msg) ->
    {succeed, _Subscribe = false, Props}.

succeed(Props, {add, Item, Self}) when Self == self(), is_pid(Item) ->
    Owner = proplists:get_value(owner, Props),
    erlmud_object:remove(Owner, item, Item),
    erlmud_object:set(Item, {owner, self()}),
    [{item, Item} | Props];
succeed(Props, {remove, Item, Self}) when Self == self(), is_pid(Item) ->
    Owner = proplists:get_value(owner, Props),
    erlmud_object:add(Owner, item, Item),
    erlmud_object:set(Item, {owner, Owner}),
    lists:keydelete(Item, 2, Props);
succeed(Props, {describe, Source, Target, AncestorsContext, _SubDescs}) ->
    _ = case is_owner(Target, Props) of
            true ->
                describe(Source, Props, AncestorsContext),
                Name = proplists:get_value(name, Props, undefined),
                BodyPartContext = <<Name/binary, " -> ">>,
                NewMessage = {describe, Source, self(), <<AncestorsContext/binary, BodyPartContext/binary>>},
                erlmud_object:attempt(Source, NewMessage);
            _ ->
                ok
        end,
    Props;
succeed(Props, Msg) ->
    log([<<"saw ">>, Msg, <<" succeed with props ">>, Props]),
    Props.

is_owner(MaybeOwner, Props) when is_pid(MaybeOwner) ->
    MaybeOwner == proplists:get_value(owner, Props);
is_owner(_, _) ->
    false.

describe(Source, Props, Context) ->
    Description = description(Props),
    erlmud_object:attempt(Source, {send, Source, [<<Context/binary>>, Description]}).

description(Props) when is_list(Props) ->
    DescTemplate = application:get_env(erlmud, body_part_desc_template, []),
    log([<<"body part desc template: ">>, DescTemplate]),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    log([<<"body part description_part RawText: ">>, RawText]),
    RawText;
description_part(Props, DescProp) ->
    log([<<"body part description_part DescProp: ">>, DescProp, <<" from Props: ">>, Props]),
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.

fail(Props, _Message, _Reason) ->
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
