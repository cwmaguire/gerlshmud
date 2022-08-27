%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_util).

-export([atob/1]).
-export([itob/1]).

atob(Atom) ->
    list_to_binary(atom_to_list(Atom)).

itob(Int) ->
    list_to_binary(integer_to_list(Int)).
