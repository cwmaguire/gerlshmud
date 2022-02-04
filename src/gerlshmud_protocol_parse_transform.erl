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

-export([write_html/1]).
%-export([write_file/1]).
-export([parse_transform/2]).
-export([escape/1]).

write_html(Filename) ->
    io:format(user, "Compiling ~p~n", [Filename]),
    Result = compile:file(Filename, [report_errors,
                                     return_errors,
                                     {parse_transform, ?MODULE},
                                     {d, filename, Filename}]),
    io:format(user, "Compile result:~n~p~n", [Result]).

parse_transform(Forms, Options) ->
    Filename = filename(Options),

    io:format(user, "Parse transforming forms: ~n"
                    "\t~p~n"
                    "\t with Options:~n"
                    "\t~p~n",
              [Forms, Options]),

    HtmlFilename = html_filename(Filename),
    io:format(user, "HtmlFilename: ~p~n", [HtmlFilename]),
    CsvFilename = csv_filename(Filename),
    io:format(user, "CsvFilename = ~p~n", [CsvFilename]),

    Events = events(Forms),

    %io:format(user, "HTML = ~p~n", [HTML]),
    %{ok, HtmlFile} = file:open(HtmlFilename, [write]),

    %case file:write(HtmlFile, [HTML, <<"\n">>]) of
        %ok ->
            %io:format(user, "Write successful~n", []);
        %Error ->
            %io:format(user, "Write failed: ~p~n", [Error])
    %end,
    %Forms.
    %
    [io:format("Event: ~p~n", [Event]) || Event <- Events],

    {ok, CsvFile} = file:open(CsvFilename, [write, append]),
    case file:write(CsvFile, [Events, <<"\n">>]) of
        ok ->
            io:format(user, "Write successful~n", []);
        Error ->
            io:format(user, "Write failed: ~p~n", [Error])
    end,
    file:close(CsvFile),
    Forms.

filename(Options) ->
    case [Filename || {d, filename, Filename} <- Options] of
        [] ->
            "protocol";
        [Filename | _] ->
            Filename % test comment
    end.

html_filename(Filename) ->
    filename:rootname(Filename) ++ ".html".

csv_filename(Filename) ->
    filename:rootname(Filename) ++ ".csv".


escape(String0) ->
    % TODO might need to "no_parse" these
    Lt = <<"<">>,
    HtmlLt = <<"&lt;">>,
    Gt = <<">">>,
    HtmlGt = <<"&gt;">>,
    % TODO This is showing up without the backslash
    Quote = <<"\"">>,
    HtmlQuote = <<"&quot;">>,
    Replacements = [{Gt, HtmlGt},
                    {Lt, HtmlLt},
                    {Quote, HtmlQuote}],

    lists:foldl(fun({Old, New}, String) ->
                    string:replace(String, Old, New, all)
                end, String0, Replacements).

events(Forms) when is_list(Forms) ->
    lists:flatten([events(Form) || Form <- Forms]);

events({function,_Line, Name,_Arity,Clauses}) when Name == 'attempt' ->
    lists:map(fun(Clause) -> attempt_clause(Name, Clause) end, Clauses);
events({function,_Line, Name,_Arity,Clauses}) when Name == 'succeed' ->
    lists:map(fun(Clause) -> succeed_clause(Name, Clause) end, Clauses);
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

clause(Name, {clause, _Line, _Head, _GuardGroups, Body}) ->
    io:format("Got non-event clause with name ~p~n", [Name]),

    % Don't look at function arguments and guards that aren't attempt or succeed
    %head(Head) ++
    %lists:map(fun guard_group/1, GuardGroups) ++

    % but do look for any calls to gerlshmud_object:attempt/2 calls
    lists:map(fun expr/1, Body).

%% I don't think you can get a function clause without a name
%attempt_clause({clause, _Line, Head, GuardGroups, Body}) ->
    %attempt_clause('', {clause, _Line, Head, GuardGroups, Body}).

attempt_clause(Name, {clause, _Line, Head, GuardGroups, Body}) ->
    io:format("Got event clause with name ~p~n", [Name]),

    [{tuple, _Line, [Parents, Props, Event]}] = Head,
    Attempt = [<<"attempt|">>,
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

succeed_clause(Name, {clause, _Line, Head, GuardGroups, Body}) ->
    io:format("Got event clause with name ~p~n", [Name]),

    [{tuple, _Line, [Props, Event]}] = Head,
    Succeed = [<<"succeed|">>,
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

case_clause({clause, _Line, [Head], GuardGroups, Body}) ->
     expr(Head) ++
     case GuardGroups of
         [] ->
             [];
         _ ->
             guard_groups(GuardGroups)
     end ++
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
    [<<"new|">>, bin(Arg1), NoProps, <<"|">>, bin(Arg2), <<"\n">>];

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
    a2b(VarName);
bin({atom, _Line, Atom}) ->
    a2b(Atom);
bin({record, _Line, parents, Fields}) ->
    [<<"#parents{">> | map_separate(fun bin/1, Fields)] ++ [<<"}">>];
bin({record_field, _Line, {atom, _Line, FieldName}, {var, _VarLine, VarName}}) ->
    [a2b(FieldName), <<" = ">>, a2b(VarName)];
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
