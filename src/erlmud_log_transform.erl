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


parse_transform(Forms, _Options) ->
    io:format(user, ".~n", []),
    {attribute, _, file, {Path, _}} = lists:keyfind(file, 3, Forms),
    io:format(user, "Path = ~p~n", [Path]),

    % Get the keys for the properties we want to log from the function specs
    LogKeys = lists:foldl(fun spec_log_keys/2, #{}, Forms),

    % for any function with message values that match a spec add a function
    % building a proplist with the spec type names as keys and the message
    % values as the values
    {Forms2, _} = lists:foldl(fun add_log_to_fun/2, {[], LogKeys}, Forms),
    lists:reverse(Forms2).

spec_log_keys({attribute, _, spec, {{Stage, _}, Funs}}, Map)
  when Stage == attempt;
       Stage == succeed;
       Stage == fail ->
    LogKeys = [fun_log_keys(Fun) || Fun <- Funs],
    maps:put(Stage, LogKeys, Map);
spec_log_keys(_, Map) ->
    Map.

add_log_to_fun(Fun = {function, X, Stage, Y, Clauses},
               {NewForms, LogKeys})
  when Stage == attempt;
       Stage == succeed;
       Stage == fail ->
    case maps:get(Stage, LogKeys, undefined) of
        undefined ->
            {[Fun | NewForms], LogKeys};
        FunLogKeys ->
            {Clauses2, FunLogKeys2}
                = lists:foldl(fun add_log_to_clause/2, {[], FunLogKeys}, Clauses),
            LogKeys2 = update_log_keys(Stage, FunLogKeys2, LogKeys),
            NewFun = {function, X, Stage, Y, lists:reverse(Clauses2)},
            NewForms2 = [NewFun | NewForms],
            {NewForms2, LogKeys2}
    end;
add_log_to_fun(Form, {NewForms, LogKeys}) ->
    {[Form | NewForms], LogKeys}.

add_log_to_clause(Clause =
                  {clause, L1,
                   _Params = [{tuple, L2, [Parents,
                                           Props,
                                           Msg = {tuple, _, Vars}]}],
                   [],
                   Expressions},
                  {Clauses, [Keys | LogKeys]})
  when length(Vars) == length(Keys) ->
    io:format("Clause matched:~nClause: ~p~nLog Keys: ~p~n", [Clause, [Keys | LogKeys]]),

    LogExpression = log_expression(Keys, Vars),
    Clause2 = {clause, L1,
               [{tuple, L2,
                 [Parents,
                  Props,
                  Msg]}],
               [],
               [LogExpression | Expressions]},

    io:format(user, "new clause = ~p~n", [Clause2]),
    {[Clause2 | Clauses], LogKeys};
add_log_to_clause(Clause, {Clauses, LogKeys}) ->
    io:format("Clause did not match:~nClause: ~p~nLog Keys: ~p~n", [Clause, LogKeys]),
    {[Clause | Clauses], LogKeys}.

log_expression(LogTypes, Vars) ->
    Cons = log_expression(lists:reverse(LogTypes),
                          lists:reverse(Vars),
                          {nil, 40}),
    {cons, X, _Hd, _Tl} = Cons,
    {match, X,
     {var,X,'Log'},
     Cons}.

log_expression([], [], Cons) ->
    Cons;
log_expression([{user_type, Type} | LogTypes],
               [{var, Y, Var} | Vars],
               Cons) ->
    Cons2 =
        {cons, Y,
         {tuple, Y, [{atom, Y, Type},
                     {var, Y, Var}]},
         Cons},
    log_expression(LogTypes, Vars, Cons2);
log_expression([{user_type, Type} | LogTypes],
               [{atom, Y, Atom} | Vars],
               Cons) ->
    Cons2 =
        {cons, Y,
         {tuple, Y, [{atom, Y, Type},
                     {atom, Y, Atom}]},
         Cons},
    log_expression(LogTypes, Vars, Cons2);
log_expression([{type, atom}  | LogTypes],
               [_NotLogged | Vars],
               Cons) ->
    log_expression(LogTypes, Vars, Cons).

update_log_keys(Stage, [], LogKeys) ->
    maps:remove(Stage, LogKeys);
update_log_keys(Stage, Keys, LogKeys) ->
    maps:update(Stage, Keys, LogKeys).

fun_log_keys(_Attempt =
             {type, _, 'fun',
              [{type, _, product,
                [{type, _, tuple,
                  [_, _, {type, _, tuple, Types}]}]},
               _]}) when is_list(Types) ->
    io:format(user, "_Attempt = ~p~n", [_Attempt]),
    io:format(user, "Types = ~p~n", [Types]),
    [{TypeType, Type} || {TypeType, _, Type, _} <- Types];
fun_log_keys(_Succeed =
             {type, _, 'fun',
              [{type, _, product,
                [{type, _, tuple,
                  [_Props, _Msg = {type, _, tuple, Types}]}]},
               _]}) when is_list(Types) ->
    [{TypeType, Type} || {TypeType, _, Type, _} <- Types];
fun_log_keys(_Fail =
             {type, _, 'fun',
              [{type, _, product,
                [{type, _, tuple,
                  [_Props, _Reason, _Msg = {type, _, tuple, Types}]}]},
               _]}) when is_list(Types) ->
    [{TypeType, Type} || {TypeType, _, Type, _} <- Types];
fun_log_keys(_Other) ->
    [].
