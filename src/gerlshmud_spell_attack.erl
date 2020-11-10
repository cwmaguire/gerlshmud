%% Copyright (c) 2020, Chris Maguire <cwmaguire@protonmail.com>
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
-module(gerlshmud_spell_attack).
-behaviour(gerlshmud_attack).

-export([should_attack/1]).

-include("include/gerlshmud.hrl").

should_attack(Props) ->
    Name = proplists:get_value(name, Props),
    Message = <<Name/binary, " is not memorized">>,
    IsMemorized = proplists:get_value(is_memorized, Props),
    IsAttack = proplists:get_value(is_attack, Props),
    IsMemorized andalso IsAttack orelse {false, Message}.
