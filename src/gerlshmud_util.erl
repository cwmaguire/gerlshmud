%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_util).

-export([atob/1]).
-export([itob/1]).
-export([describe/2]).

atob(Atom) ->
    list_to_binary(atom_to_list(Atom)).

itob(Int) ->
    list_to_binary(integer_to_list(Int)).

describe(Template, Props) ->
    DescTemplate = gerlshmud_config:desc_template(Template),
    [[description_part(Props, Part)] || Part <- DescTemplate].

description_part(_, RawText) when is_binary(RawText) ->
    RawText;
description_part(Props, DescProp) ->
    prop_description(proplists:get_value(DescProp, Props, <<"??">>)).

prop_description(undefined) ->
    [];
prop_description(Value) when not is_pid(Value) ->
    Value.
