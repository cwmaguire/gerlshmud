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
-module(erlmud_attempt_char_look).
-behaviour(erlmud_attempt).

-export([attempt/1]).

attempt({_Owner, Props, {look, Source, TargetName}}) when Source =/= self(),
                                                  is_binary(TargetName) ->
    log(debug, [<<"Checking if name ">>, TargetName, <<" matches">>]),
    SelfName = proplists:get_value(name, Props, <<>>),
    case re:run(SelfName, TargetName, [{capture, none}, caseless]) of
        match ->
            Context = <<SelfName/binary, " -> ">>,
            NewMessage = {describe, Source, self(), deep, Context},
            {{resend, Source, NewMessage}, _ShouldSubscribe = true, Props};
        _ ->
            ct:pal("Name ~p did not match this character's name ~p~n", [TargetName, SelfName]),
            log(debug,
                [<<"Name ">>,
                 TargetName,
                 <<" did not match this character's name: ">>,
                 SelfName,
                 <<".\n">>]),
            {succeed, false, Props}
    end;
attempt({Room = _Owner, Props,
        _JustPlainLook = {look, SelfSource}})
  when SelfSource == self() ->
    NewMessage = {look, SelfSource, Room},
    {{resend, SelfSource, NewMessage}, _ShouldSubscribe = false, Props};
attempt({OwnerRoom, Props,
        _DescFromParent = {describe, _Source, OwnerRoom, _RoomContext}}) ->
    {succeed, true, Props};
attempt(_) ->
    not_interested.

log(Level, IoData) ->
    erlmud_event_log:log(Level, [list_to_binary(atom_to_list(?MODULE)) | IoData]).
