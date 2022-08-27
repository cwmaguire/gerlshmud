%% Copyright 2022, Chris Maguire <cwmaguire@protonmail.com>
-module(gerlshmud_data).

-export([fetch/1]).

fetch(player1) ->
    {gerlshmud_player, player1, [{room, room1}, {item, item1}]};
fetch(_) ->
    undefined.
