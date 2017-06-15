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
-module(erlmud_parse).

-export([parse/2]).

parse(Player, <<"n">>) ->
    log([<<"Moving n">>]),
    {move, Player, n};
parse(Player, <<"get ", Item/binary>>) ->
    log([<<"Getting ">>, Item]),
    {get, Player, Item};
parse(Player, <<"drop ", Item/binary>>) ->
    log([<<"Dropping ">>, Item]),
    {drop, Player, Item};
parse(Player, <<"look">>) ->
    log([<<"Looking ">>]),
    {look, Player};
parse(Player, <<"look ", Object/binary>>) ->
    log([<<"Looking ">>, Object]),
    {look, Player, Object};
parse(_, _) ->
    {error, <<"Huh?">>}.

log(_Terms) ->
    ok.
    %erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
