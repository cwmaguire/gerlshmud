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
-module(erlmud_handler_room_move).
-behaviour(erlmud_handler).

-export([attempt/1]).
-export([succeed/1]).
-export([fail/1]).

attempt({_Owner, Props, {_Obj, move, from, Source, to, Target, via, _Exit}}) when Source == self(); Target == self() ->
    {succeed, true, Props};
attempt(_) ->
    undefined.

succeed({Props, {Obj, move, from, Self, to, Target, via, _Exit}}) when Self == self() ->
    log([Obj, <<" went to ">>, Target]),
    lists:keydelete(Obj, 2, Props);
succeed({Props, {Obj, move, from, Source, to, Self, via, _Exit}}) when Self == self() ->
    log([Obj, <<" came from ">>, Source]),
    [{character, Obj} | Props];
succeed({Props, {Obj, move, from, Source, to, Target}}) ->
    log([<<"Process ">>, Obj, <<" went from ">>, Source, <<" to ">>, Target]),
    Props;
succeed({Props, _}) ->
    Props.

fail({Props, Reason, {Obj, move, from, Self, to, Target}}) when Self == self() ->
    log([Obj, <<" couldn't go from here to ">>, Target, <<" ">>, Reason]),
    Props;
fail({Props, Reason, {Obj, move, from, Source, to, Target, via, Exit}}) when Source == self(); Target == self() ->
    log([Obj, <<" couldn't move from ">>,
         <<" room ">>, Source, <<" to room ">>, Target,
         <<" via ">>, Exit,
         <<" because ">>, Reason,
         <<", Props: ">>, Props]),
    Props;
fail({Props, _, _}) ->
    Props.

log(Terms) ->
    erlmud_event_log:log(debug, [list_to_binary(atom_to_list(?MODULE)) | Terms]).
