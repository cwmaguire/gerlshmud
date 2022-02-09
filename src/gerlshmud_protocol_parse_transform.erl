%% Copyright (c) 2022, Chris Maguire <cwmaguire@gmail.com>
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
-module(gerlshmud_protocol_parse_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    io:format("~~", []),

    start_module_proc(Forms),

    CsvFilename = csv_filename("protocol"),

    Events = events(Forms),

    {ok, CsvFile} = file:open(CsvFilename, [write, append]),
    case file:write(CsvFile, [Events]) of
        ok ->
            %io:format(user, "Write successful~n", []);
            ok;
        Error ->
            io:format(user, "Write failed: ~p~n", [Error])
    end,
    file:close(CsvFile),
    stop_module_proc(),
    Forms.

start_module_proc(Forms) ->
    Module = module(Forms),
    io:format("~p module process for ~p starting~n", [self(), Module]),
    ModuleProcFun =
        fun() ->
            Fun = fun(Module_, Self) ->
                      receive
                          {Pid, module} ->
                              %io:format("~p sending back module name ~p~n", [self(), Module_]),
                              Pid ! {module, Module},
                              Self(Module_, Self);
                          die ->
                              io:format("~p module process for ~p dying~n", [self(), Module_]),
                              ok
                      end
                  end,
            Fun(Module, Fun)
        end,
    ModuleProc = spawn(ModuleProcFun),
    true = register(module, ModuleProc).

stop_module_proc() ->
    module ! die.

module([{attribute, _Line, module, Module} | _]) ->
    remove_prefix(<<"gerlshmud_handler_">>, a2b(Module));
module([_ | Forms]) ->
    module(Forms);
module(_) ->
    <<"unknown">>.

module() ->
    module ! {self(), module},
    receive
        {module, Module} ->
            Module
    after 10 ->
        <<"no_module">>
    end.

remove_prefix(Prefix, Bin) ->
    case Bin of
        <<Prefix:(size(Prefix))/binary, Rest/binary>> ->
            Rest;
        Bin ->
            Bin
    end.

csv_filename(Filename) ->
    filename:rootname(Filename) ++ ".csv".

events(Forms) when is_list(Forms) ->
    lists:flatten([events(Form) || Form <- Forms]);
events({function,_Line, Name,_Arity,Clauses}) when Name == 'attempt' ->
    lists:map(fun(Clause) -> attempt_clause(Clause) end, Clauses);
events({function,_Line, Name,_Arity,Clauses}) when Name == 'succeed' ->
    lists:map(fun(Clause) -> succeed_clause(Clause) end, Clauses);
events(_) ->
    [].


catch_clause({clause, _Line, Exception, GuardGroups, Body}) ->
    [{tuple, _Line, [Class, ExceptionPattern, _Wild]}] = Exception,
    expr(Class) ++
    expr(ExceptionPattern) ++
    guard_groups(GuardGroups) ++
    lists:map(fun expr/1, Body).

clause({clause, _Line, Head, GuardGroups, Body}) ->
    clause('', {clause, _Line, Head, GuardGroups, Body}).

clause(_Name, {clause, _Line, _Head, _GuardGroups, Body}) ->
    % Don't look at function arguments and guards that aren't attempt or succeed
    % but do look for any calls to gerlshmud_object:attempt/2 calls
    lists:map(fun expr/1, Body).

%% We don't need to see catch-all clauses in the protocol
%% attempt(_) -> ...
attempt_clause({clause, _Line1, [{var, _Line2, '_'}], _, _}) ->
    [];

%% We don't need to see catch-all clauses in the protocol
%% attempt(_Var) -> ...
attempt_clause({clause, _Line1, [{var, _Line2, Var}], _, _}) when Var == '_Attempt'; Var == '_Msg' ->
    [];

%% We don't need to see catch-all clauses in the protocol
%% attempt({_, _, _Msg}) -> ...
attempt_clause({clause,
                _Line1,
                [{tuple, _Line2, [{var, _Line3, '_'}, {var, _Line4, '_'}, {var, _Line5, '_Msg'}]}],
                _GuardGroups,
                _Body}) ->
    [];

%% We don't need to see catch-all clauses in the protocol
%% attempt(_Foo = {_, _, _Msg}) -> ...
attempt_clause({clause,
                _Line1,
                [{match, _Line2, {var, _, _}, {tuple, _Line3, [{var, _Line4, '_'}, {var, _Line5, '_'}, {var, _Line6, '_Msg'}]}}],
                _GuardGroups,
                _Body}) ->
    [];

%% Strip off any Variable that the event is bound too: it screws up the sorting of events and we're just
%% interested in the events themselves, not what they're bound to.
%% attempt(Parents, Props, Message = {Bar, baz, Quux}) -> ...
attempt_clause({clause,
                Line1,
                [{tuple, Line2, [Parents, Props, {match, _Line3, {var, _, _}, Event}]}],
                GuardGroups,
                Body}) ->
    attempt_clause({clause, Line1, [{tuple, Line2, [Parents, Props, Event]}], GuardGroups, Body});

attempt_clause({clause, _Line, Head, GuardGroups, Body}) ->
    [{tuple, _Line2, [Parents, Props, Event]}] = Head,
    Attempt = [module(),
               <<"|">>,
               <<"attempt|">>,
               attempt_head(Parents, Props, Event),
               <<"|">>,
               guard_groups(GuardGroups),
               <<"\n">>],
    %head(Head)
    %lists:map(fun guard_group/1, GuardGroups) ++

    [Attempt] ++
    lists:map(fun expr/1, Body).

attempt_head(Parents, Props, Event) ->
    %{Parents, Props, Event}.
    separate(<<"|">>, [bin(Parents), bin(Props), bin(Event)]).

%% I don't think you can get a function clause without a name
%succeed_clause({clause, _Line, Head, GuardGroups, Body}) ->
    %succeed_clause('', {clause, _Line, Head, GuardGroups, Body}).

%% We don't need to see catch-all clauses in the protocol
%% succeed({AnyVAr, _}) -> ...
succeed_clause({clause, _Line2, [{tuple, _Line2, [_Props, {var, _Line3, Ignored}]}], _, _})
  when Ignored == '_';
       Ignored == '_Msg';
       Ignored == '_Other' ->
    [];

succeed_clause({clause, _Line2, [{tuple, _Line2, [Props, {match, _Line3, {var, _Line3, 'Msg'}, Event}]}], GuardGroups, Body}) ->
    succeed_clause({clause, 0, [{tuple, 0, [Props, Event]}], GuardGroups, Body});

succeed_clause({clause, _Line, Head, GuardGroups, Body}) ->
    %io:format("Got event clause with name ~p~n", [Name]),

    [{tuple, _Line, [Props, Event]}] = Head,
    Succeed = [module(),
               <<"|">>,
               <<"succeed|">>,
               _NoParents = <<"|">>,
               succeed_head(Props, Event),
               <<"|">>,
               guard_groups(GuardGroups),
               <<"\n">>],

    [Succeed] ++
    lists:map(fun expr/1, Body).

succeed_head(Props, Event) ->
    %{Props, Event}.
    separate(<<"|">>, [bin(Props), bin(Event)]).

case_clause({clause, _Line, [Head], _GuardGroups, Body}) ->
     expr(Head) ++

     % I don't think we can call gerlshmud_object:attempt/2 in a guard clause
     % and the only reason to descend below function heads is to look for calls
     % to gerlshmud_object:attempt/2.
     %case GuardGroups of
     %    [] ->
     %        [];
     %    _ ->
     %        guard_groups(GuardGroups)
     %end ++
     lists:map(fun expr/1, Body).

%head(Expressions) ->
    %io:format("Got function head: ~p~n", [Expressions]),
    %lists:map(fun expr/1, Expressions).

%% -type pattern(Pattern) -> Pattern.
%%  N.B. Only valid patterns are included here.
expr({lc,_Line,Result,Quals}) ->
    Events1 = expr(Result),
    Events2 = lists:map(fun lc_bc_qual/1, Quals),
    Events1 ++ Events2;
expr({bc,_Line,E0,Quals}) ->
    Events1 = expr(E0),
    Events2 = lists:map(fun lc_bc_qual/1, Quals),
    Events1 ++ Events2;
expr({block,_Line,Expressions}) ->
    lists:map(fun expr/1, Expressions);
expr({'if',_Line,Clauses}) ->
    lists:map(fun clause/1, Clauses);
expr({'case',_Line,Expression,Clauses}) ->
    Events1 = expr(Expression),
    Events2 = lists:map(fun case_clause/1, Clauses),
    Events1 ++ Events2;
expr({'receive',_Line,Clauses}) ->
    lists:map(fun clause/1, Clauses);
expr({'receive',_Line,Clauses,AfterWait,AfterExpressions}) ->
    Events1 = lists:map(fun clause/1, Clauses),
    Events2 = expr(AfterWait),
    Events3 = lists:map(fun expr/1, AfterExpressions),
    Events1 ++ Events2 ++ Events3;
expr({'try',_Line,Expressions,_WhatIsThis,CatchClauses,AfterExpressions}) ->
    Events1 = lists:map(fun expr/1, Expressions),
    Events2 = lists:map(fun catch_clause/1, CatchClauses),
    Events3 = lists:map(fun expr/1, AfterExpressions),
    Events1 ++ Events2 ++ Events3;
expr({'fun',_Line,Body}) ->
    case Body of
        {clauses,Clauses} ->
            lists:map(fun(Clause) -> clause('', Clause) end, Clauses);
        _ ->
            []
    end;
%expr({call,_Line,Fun,Args}) ->
expr({call, _Line,
      {remote, _RemLine,
       {atom, _AtomLine, gerlshmud_object},
       {atom, _FunAtomLine, attempt}},
      [Arg1, Arg2]}) ->
    %NoParents = <<"|">>,
    NoProps = <<"|">>,
    [module(), <<"|new|">>, bin(Arg1), NoProps, <<"|">>, bin(Arg2), <<"\n">>];

expr({call,_Line,__Fun, _Args}) ->
    %io:format("Got call with fun ~p and args ~p~n", [_Fun, Args]),
    %% N.B. If F an atom then call to local function or BIF, if F a
    %% remote structure (see below) then call to other module,
    %% otherwise apply to "function".
     %io:format(user, "calling expr(~p)~n", [Fun]),
    %lists:map(fun expr/1, Args);

    % Don't care about non-event calls
    [];
expr({'catch',_Line,Expression}) ->
    %% No new variables added.
    expr(Expression);
expr({match,_Line,Expr1,Expr2}) ->
    expr(Expr1) ++ expr(Expr2);
expr({op,_Line,'==',L,R}) ->
    expr(L) ++ expr(R);
expr({op, _Line, _Op, L, R}) ->
    expr(L) ++ expr(R);
expr({tuple,_Line,Exprs}) ->
    lists:map(fun expr/1, Exprs);
%% There's a special case for all cons's after the first: {tail, _}
%% so this is a list of one item.
expr({cons,_Line,Head,{nil, _}}) ->
    expr(Head);
expr({cons,_Line,Head,{var, _Line2, '_'}}) ->
    expr(Head);
expr(_Cons = {cons,_Line,Head,Tail}) ->
    %io:format(user, "Cons -> Tail = ~p~n", [Cons]),
    expr(Head) ++  expr({tail, Tail});
expr(_Tail = {tail, {cons, _Line, Head, {nil, _}}}) ->
    %io:format(user, "Tail 1 = ~p~n", [Tail]),
    expr(Head);
expr(_Tail_ = {tail, {cons, _Line, Head, Tail}}) ->
    %io:format(user, "Tail 2 = ~p~n", [Tail_]),
    expr(Head) ++ expr({tail, Tail});
expr({tail, Call = {call, _Line, _Fun, _Args}}) ->
     expr(Call);
expr({tail, Tail}) ->
    %io:format(user, "Unknown tail ~p~n", [Unknown]);
    expr(Tail);
%expr({tail, Unknown}) ->
    %io:format(user, "Unknown tail ~p~n", [Unknown]);
expr({record, _Line, _Name, ExprFields}) ->
    lists:map(fun expr_field/1, ExprFields);
expr({record_index,_Line, _Name, Field}) ->
     expr(Field);
expr({record_field,_Line,Expression, _RecName, Field}) ->
    expr(Expression) ++ expr(Field);
% How does this happen? (Foo).bar ?
%expr({record_field,Line,Rec0,Field0}) ->
    %Rec1 = expr(Rec0),
    %Field1 = expr(Field0);
expr(_IgnoredExpr) ->
    %io:format(user, "Ignored expr:~n~p~n", [IgnoredExpr]),
    [].

expr_field({record_field, _Lf, {atom, _La, _F}, Expr}) ->
    expr(Expr);
expr_field({record_field, _Lf, {var,_La,'_'}, Expr}) ->
    expr(Expr).

guard_groups(GuardGroups) ->
    map_separate(<<"; ">>, fun guard_group_conjunction/1, GuardGroups).

guard_group_conjunction(GuardGroupConjunctionExpressions) ->
    map_separate(<<", ">>, fun bin/1, GuardGroupConjunctionExpressions).

%% This is a list of generators _or_ filters
%% which are simply expressions
%% A generator is a target and a source
lc_bc_qual({generate,_Line,Target,Source}) ->
    expr(Target) ++ expr(Source);
lc_bc_qual({b_generate,_Line,Target,Source}) ->
    expr(Target) ++ expr(Source);
lc_bc_qual(FilterExpression) ->
    expr(FilterExpression).

bin({var, _Line, VarName}) ->
    move_leading_underscore(a2b(VarName));

bin({atom, _Line, Atom}) ->
    a2b(Atom);

bin({integer, _Line, Int}) ->
    integer_to_binary(Int);

bin({match, _Line, Var, Tuple}) ->
    [bin(Var), <<" = ">>, bin(Tuple)];

bin({record, _Line, RecordName, Fields}) ->
    [<<"#">>, a2b(RecordName), <<"{">> | map_separate(fun bin/1, Fields)] ++ [<<"}">>];

bin({record_field, _Line, {atom, _Line2, FieldName}, {var, _VarLine, VarName}}) ->
    [a2b(FieldName), <<" = ">>, a2b(VarName)];

bin({record_field, _Line, {atom, _Line2, FieldName}, {match, _Line3, Var, Record}}) ->
    [a2b(FieldName), <<" = ">>, bin(Var), <<" = ">>, bin(Record)];

bin({record_field, _Line, {atom, _Line2, FieldName}, Call}) ->
    [a2b(FieldName), <<" = ">>, bin(Call)];

bin({tuple, _Line, Expressions}) ->
    [<<"{">> | map_separate(fun bin/1, Expressions)] ++ [<<"}">>];

bin({op, _Line, Operator, Expr1, Expr2}) ->
    [bin(Expr1), <<" ">>, a2b(Operator), <<" ">>, bin(Expr2)];

bin({call, _Line, {atom, _Line2, FunctionName}, Params}) ->
    ParamBins = map_separate(<<", ">>, fun bin/1, Params),
    [a2b(FunctionName), <<"(">>, ParamBins, <<")">>].


%separate(List) when is_list(List) ->
    %separate(<<", ">>, List).

separate(Separator, List) ->
    lists:join(Separator, List).

map_separate(Fun, List) ->
    map_separate(<<", ">>, Fun, List).

map_separate(Separator, Fun, List) ->
    separate(Separator, lists:map(Fun, List)).

a2b(Atom) ->
    list_to_binary(atom_to_list(Atom)).

move_leading_underscore(JustUnderscore = <<$_>>) ->
    JustUnderscore;
move_leading_underscore(<<$_, Rest/binary>>) ->
    <<Rest/binary, "_">>;
move_leading_underscore(Bin) ->
    Bin.
