%% Copyright (c) 2019, Chris Maguire <cwmaguire@gmail.com>
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
-module(erlmud_log_transform).

-export([parse_transform/2]).


parse_transform(Forms, Options) ->
    io:format(user, ".~n", []),
    {attribute, _, file, {Path, _}} = lists:keyfind(file, 3, Forms),
    case Path of
        "src/erlmud_handler_attribute_look.erl" ->
            io:format(user, "Parse transforming forms: ~n"
                            "\t~p~n"
                            "\t with Options:~n"
                            "\t~p~n",
                      [Forms, Options]),
            parse(Forms);
        _ ->
            io:format(user, "Path: ~p~n", [Path]),
            Forms
    end.

parse(Forms) ->
    LogKeys = lists:foldl(fun spec_log_keys/2, #{}, Forms),
    Funs = funs(Forms),
    lists:foldl(fun add_log_message/2, {[], LogKeys}, Forms).

spec_log_keys({attribute, _, spec, {{Stage, _}, Funs}}, Map)
  when Stage == attempt;
       Stage == succeed;
       Stage == fail ->
    LogKeys = [fun_log_keys(Fun) || Fun <- Funs],
    maps:put(Stage, LogKeys, Map);
spec_log_keys(_, Map) ->
    Map.

add_log_to_fun({function, X, Stage, Y, Clauses},
                {NewForms, LogKeys})
  when Stage == attempt;
       Stage == succeed;
       Stage == fail ->
    FunLogKeys = maps:get(Stage, LogKeys),
    {Clauses2, FunLogKeys2}
        = lists:foldl(fun add_log_clause/2, {[], FunLogKeys} Forms),
    [{function, X, Stage, Y, lists:reverse(Clauses2)} | NewForms];
add_log_to_fun(Form, {NewForms, LogKeys}) ->
    {[Form | NewForms], LogKeys}.

add_log_to_clause({clause, 34,
                   [{tuple, X,
                     [Parents,
                      Props,
                      Msg = [Vars]]}],
                   [],
                   Expressions},
                  LogKeys) ->

    [LogTypes | Rest] = get(Stage, LogKeys),
    case length(LogTypes) == length(Vars) of
        true ->
            add_log_to_expressions(Expressions, LogTypes)



    add_log_to_expression

% TODO add succeed and fail functions
% This first clause works for attempt
fun_log_keys({type, _, 'fun',
              [{type, _, product,
                [{type, _, tuple,
                  [_, _, {type, _, tuple, Types}]}]},
               _]}) ->
    [{TypeType, Type} || {TypeType, _, Type, _} <- Types];
fun_log_keys(_) ->
    not_params.

fun_({function, _, Stage, _, Clauses}, Funs)
  when Stage == attempt;
       Stage == succeed;
       Stage == fail ->
    [{Stage, Clauses} | Funs];
fun_(_, Funs) ->
    Funs.
