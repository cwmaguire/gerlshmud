%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_config).

-include("include/gerlshmud.hrl").

-export([desc_template/1]).

desc_template(Type) ->
    DefaultTemplate = default_template(Type),
    DescTemplates = application:get_env(gerlshmud, desc_templates, []),
    proplists:get_value(Type, DescTemplates, DefaultTemplate).

default_template(Type) ->
    Text = atom_to_binary(Type, utf8),
    [<<Text/binary, ": ">>, value].
