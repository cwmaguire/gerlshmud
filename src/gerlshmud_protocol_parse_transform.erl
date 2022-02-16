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

    CsvFilename = csv_filename("protocol"),

    Events = events(Forms, #{module => module(Forms)}),

    {ok, CsvFile} = file:open(CsvFilename, [write, append]),
    case file:write(CsvFile, [Events]) of
        ok ->
            %io:format(user, "Write successful~n", []);
            ok;
        Error ->
            io:format(user, "~p Write failed: ~p~n~p~n", [module(Forms), Error, Events])
    end,
    file:close(CsvFile),
    Forms.

module([{attribute, _Line, module, Module} | _]) ->
    remove_prefix(<<"gerlshmud_handler_">>, a2b(Module));
module([_ | Forms]) ->
    module(Forms);
module(_) ->
    <<"unknown">>.

remove_prefix(Prefix, Bin) ->
    case Bin of
        <<Prefix:(size(Prefix))/binary, Rest/binary>> ->
            Rest;
        Bin ->
            Bin
    end.

csv_filename(Filename) ->
    filename:rootname(Filename) ++ ".csv".

events(Forms, State) when is_list(Forms) ->
    lists:flatten([events(Form, State) || Form <- Forms]);
events({function,_Line, Name,_Arity,Clauses}, State) when Name == 'attempt' ->
    lists:map(fun(Clause) -> attempt_clause(Clause, State) end, Clauses);
events({function,_Line, Name,_Arity,Clauses}, State) when Name == 'succeed' ->
    lists:map(fun(Clause) -> succeed_clause(Clause, State) end, Clauses);
events(_Form, _State) ->
    [].


catch_clause({clause, _Line, Exception, GuardGroups, Body}, State) ->
    [{tuple, _Line, [Class, ExceptionPattern, _Wild]}] = Exception,
    {Exp1, State1} = search(Class, State),
    {Exp2, State2} = search(ExceptionPattern, State1),
    {Exp3, State3} = guard_groups(GuardGroups, State2),
    {Exps, State4} = loop_with_state(Body, fun search/2, State3),
    {[Exp1, Exp2, Exp3 | Exps], State4}.

clause({clause, _Line, Head, GuardGroups, Body}, State) ->
    clause('', {clause, _Line, Head, GuardGroups, Body}, State).

clause(_Name, {clause, _Line, _Head, _GuardGroups, Body}, State) ->
    % Don't look at function arguments and guards that aren't attempt or succeed
    % but do look for any calls to gerlshmud_object:attempt/2 calls
   loop_with_state(Body, fun search/2, State).

%% We don't need to see catch-all clauses in the protocol
%% attempt(_) -> ...
attempt_clause({clause, _Line1, [{var, _Line2, '_'}], _, _}, _State) ->
    [];

%% We don't need to see catch-all clauses in the protocol
%% attempt(_Var) -> ...
attempt_clause({clause, _Line1, [{var, _Line2, Var}], _, _}, _State) when Var == '_Attempt'; Var == '_Msg' ->
    [];

%% We don't need to see catch-all clauses in the protocol
%% attempt({_, _, _Msg}) -> ...
attempt_clause({clause,
                _Line1,
                [{tuple, _Line2, [{var, _Line3, '_'}, {var, _Line4, '_'}, {var, _Line5, '_Msg'}]}],
                _GuardGroups,
                _Body}, _State) ->
    [];

%% We don't need to see catch-all clauses in the protocol
%% attempt(_Foo = {_, _, _Msg}) -> ...
attempt_clause({clause,
                _Line1,
                [{match, _Line2, {var, _, _}, {tuple, _Line3, [{var, _Line4, '_'}, {var, _Line5, '_'}, {var, _Line6, '_Msg'}]}}],
                _GuardGroups,
                _Body}, _State) ->
    [];

%% Strip off any Variable that the event is bound too: it screws up the sorting of events and we're just
%% interested in the events themselves, not what they're bound to.
%% attempt(Parents, Props, Message = {Bar, baz, Quux}) -> ...
attempt_clause({clause,
                Line1,
                [{tuple, Line2, [Parents, Props, {match, _Line3, {var, _, _}, Event}]}],
                GuardGroups,
                Body},
               State) ->
    attempt_clause({clause, Line1, [{tuple, Line2, [Parents, Props, Event]}], GuardGroups, Body}, State);

attempt_clause({clause, _Line, Head, GuardGroups, Body}, State) ->
    [{tuple, _Line2, [Parents, Props, Event]}] = Head,

    {AttemptHeadExprs, State1} = attempt_head(Parents, Props, Event, State),
    {GuardGroupExprs, State2} =  guard_groups(GuardGroups, State1),
    {BodyExprs, State3} = loop_with_state(Body, fun search/2, State2),

    Module = maps:get(module, State3),
    MaybeResentMessage = maps:get(resent_message, State3, undefined),
    MaybeBroadcastMessage = maps:get(broadcast_message, State3, undefined),

    ResentMessage =
        case MaybeResentMessage of
            undefined ->
                <<>>;
            Other ->
                [<<"resend = ">>, Other]
        end,
    BroadcastMessage =
        case MaybeBroadcastMessage of
            undefined ->
                <<>>;
            Other_ ->
                [<<"broadcast = ">>, Other_]
        end,

    Attempt = [Module, <<"|">>,
               <<"attempt|">>,
               AttemptHeadExprs, <<"|">>,
               GuardGroupExprs, <<"|">>,
               ResentMessage,
               BroadcastMessage,
               <<"\n">>],

    AttemptEvent = hd(lists:reverse(AttemptHeadExprs)),

    MaybeResentOrBroadcastEvent =
        case {MaybeResentMessage, MaybeBroadcastMessage} of
            {undefined, undefined} ->
                <<>>;
            {_, undefined} ->
                %% Module, Type, Parents, Props, Event, Guards, resend | broadcast | attempt message
                [Module, <<"|resend|||">>, MaybeResentMessage, <<"||attempt = ">>, AttemptEvent, <<"\n">>];
            {undefined, _} ->
                %% Module, Type, Parents, Props, Event, Guards, resend | broadcast | attempt message
                [Module, <<"|broadcast|||">>, MaybeBroadcastMessage, <<"||attempt = ">>, AttemptEvent, <<"\n">>]
        end,

    % I'm not sure anything can be in BodyExprs because anything that doesn't come from a attempt
    % or succeed clause is stored in the state.
    [Attempt,  BodyExprs, MaybeResentOrBroadcastEvent].

attempt_head(Parents, Props, Event, State) ->
    %{Parents, Props, Event}.
    ParentsBin = print(Parents),
    PropsBin = print(Props),
    EventBin = print(Event),
    Output = separate(<<"|">>, [ParentsBin, PropsBin, EventBin]),
    {Output, State#{event => EventBin}}.

%% I don't think you can get a function clause without a name
%succeed_clause({clause, _Line, Head, GuardGroups, Body}) ->
    %succeed_clause('', {clause, _Line, Head, GuardGroups, Body}).

%% We don't need to see catch-all clauses in the protocol
%% succeed({AnyVAr, _}) -> ...
succeed_clause({clause, _Line2, [{tuple, _Line2, [_Props, {var, _Line3, Ignored}]}], _, _}, _State)
  when Ignored == '_';
       Ignored == '_Msg';
       Ignored == '_Other' ->
    [];

succeed_clause({clause, _Line2, [{tuple, _Line2, [Props, {match, _Line3, {var, _Line3, 'Msg'}, Event}]}], GuardGroups, Body}, State) ->
    succeed_clause({clause, 0, [{tuple, 0, [Props, Event]}], GuardGroups, Body}, State);

succeed_clause({clause, _Line, Head, GuardGroups, Body}, State) ->
    [{tuple, _Line, [Props, Event]}] = Head,

    {SucceedHeadExprs, State1} = succeed_head(Props, Event, State),
    {GuardGroupsExprs, State2} = guard_groups(GuardGroups, State1),

    Succeed = [maps:get(module, State2),
               <<"|">>,
               <<"succeed|">>,
               _NoParents = <<"|">>,
               SucceedHeadExprs,
               <<"|">>,
               GuardGroupsExprs,
               <<"\n">>],

    {BodyExprs, _State} = loop_with_state(Body, fun search/2, State2),
    [Succeed | BodyExprs].

succeed_head(Props, Event, State) ->
    PropBin = print(Props),
    EventBin = print(Event),

    CSVOutput = separate(<<"|">>, [PropBin, EventBin]),
    {CSVOutput, State}.

case_clause({clause, _Line, [Head], _GuardGroups, Body}, State) ->
     {HeadExprs, State1} = search(Head, State),

     % I don't think we can call gerlshmud_object:attempt/2 in a guard clause
     % and the only reason to descend below function heads is to look for calls
     % to gerlshmud_object:attempt/2.
     %case GuardGroups of
     %    [] ->
     %        [];
     %    _ ->
     %        guard_groups(GuardGroups)
     %end ++

    {BodyExprs, State2} = loop_with_state(Body, fun search/2, State1),

    {[HeadExprs | BodyExprs], State2}.

search({lc,_Line,Result,Quals}, State) ->
    {Events1, State1} = search(Result, State),
    {Events2, State2} = loop_with_state(Quals, fun lc_bc_qual/2, State1),
    {[Events1 | Events2], State2};
search({bc,Line,E0,Quals}, State) ->
    search({lc, Line, E0, Quals}, State); %% other than 'bc', this is the same as the clause above
search({block,_Line,Expressions}, State) ->
    loop_with_state(Expressions, fun search/2, State);
search({'if',_Line,Clauses}, State) ->
    loop_with_state(Clauses, fun clause/2, State);
search({'case',_Line,Expression,Clauses}, State) ->

    {Events1, State1} = search(Expression, State),
    {Events2, State2} = loop_with_state(Clauses, fun case_clause/2, State1),
    {[Events1 | Events2], State2};
search({'receive',_Line,Clauses}, State) ->
    loop_with_state(Clauses, fun clause/2, State);
search({'receive',_Line,Clauses,AfterWait,AfterExpressions}, State) ->
    {Events1, State1} = loop_with_state(Clauses, fun clause/2, State),
    {Events2, State2} = search(AfterWait, State1),
    {Events3, State3} = loop_with_state(AfterExpressions, fun search/2, State2),
    {[Events1 | [Events2 | Events3]], State3};
search({'try',_Line,Expressions,_WhatIsThis,CatchClauses,AfterExpressions}, State) ->
    {Events1, State1} = loop_with_state(Expressions, fun search/2, State),
    {Events2, State2} = loop_with_state(CatchClauses, fun catch_clause/2, State1),
    {Events3, State3} = loop_with_state(AfterExpressions, fun search/2, State2),
    {[Events1 | [Events2 | Events3]], State3};

search({'fun',_Line,Body}, State) ->
    case Body of
        {clauses,Clauses} ->
            Fun = fun(Clause) -> clause('', Clause) end,
            loop_with_state(Clauses, Fun, State);
        _ ->
            {[], State}
    end;
%search({call,_Line,Fun,Args}) ->
search({call, _Line,
      {remote, _RemLine,
       {atom, _AtomLine, gerlshmud_object},
       {atom, _FunAtomLine, attempt}},
      [Arg1, Arg2]},
     State) ->
    %NoParents = <<"|">>,
    NoProps = <<"|">>,
    Arg1Bin = print(Arg1),
    Arg2Bin = print(Arg2),
    {[maps:get(module, State), <<"|new|">>, Arg1Bin, NoProps, <<"|">>, Arg2Bin, <<"\n">>], State};

search({call, _Line,
      {remote, _RemLine,
       {atom, _AtomLine, gerlshmud_object},
       {atom, _FunAtomLine, attempt_after}},
      [Arg1, _, Arg3]}, State) ->
    NoProps = <<"|">>,
    Arg1Bin = print(Arg1),
    Arg3Bin = print(Arg3),
    {[maps:get(module, State), <<"|new|">>, Arg1Bin, NoProps, <<"|">>, Arg3Bin, <<"\n">>], State};

search({call,_Line,__Fun, _Args}, State) ->
    % Don't care about non-event calls
    {[], State};
search({'catch',_Line,Expression}, State) ->
    %% No new variables added.
    search(Expression, State);

search({match, _Line, {var, _Line1, 'NewMessage'}, NewMessage}, State) ->
    % we can ignore the var, since we know we won't need it in the CSV
    NewMessageBin = print(NewMessage),
    {[], State#{new_message => NewMessageBin}};

search({match,_Line,Expr1,Expr2}, State) ->
    {Expr1Expr, State1} = search(Expr1, State),
    {Expr2Expr, State2} = search(Expr2, State1),
    {[Expr1Expr | [Expr2Expr]], State2};

search({op,_Line,'==',L,R}, State) ->
    {LExpr, State1} = search(L, State),
    {RExpr, State2} = search(R, State1),
    {[LExpr | [RExpr]], State2};

search({op, _Line, _Op, L, R}, State) ->
    {LExpr, State1} = search(L, State),
    {RExpr, State2} = search(R, State1),
    {[LExpr | [RExpr]], State2};

search({tuple, _Line, [{atom, _Line1, resend}, Source, {var, _Line1, 'NewMessage'}]}, State = #{new_message := NewMessage}) ->
    {SourceExpr, State1} = search(Source, State),
    {SourceExpr, State1#{resent_message => NewMessage}};

search({tuple, _Line, [{atom, _Line1, broadcast}, {var, _Line1, 'NewMessage'}]}, State = #{new_message := NewMessage}) ->
    {[], State#{broadcast_message => NewMessage}};

search({tuple,_Line, TupleExpressions}, State) ->
    loop_with_state(TupleExpressions, fun search/2, State);
%% There's a special case for all cons's after the first: {tail, _}
%% so this is a list of one item.
search({cons,_Line,Head,{nil, _}}, State) ->
    search(Head, State);
search({cons,_Line,Head,{var, _Line2, '_'}}, State) ->
    search(Head, State);
search(_Cons = {cons,_Line,Head,Tail}, State) ->
    {HeadExpr, State1} = search(Head, State),
    {TailExpr, State2} = search(Tail, State1),
    {[HeadExpr | [TailExpr]], State2};
search(_Tail = {tail, {cons, _Line, Head, {nil, _}}}, State) ->
    search(Head, State);
search(_Tail_ = {tail, {cons, _Line, Head, Tail}}, State) ->
    {HeadExpr, State1} = search(Head, State),
    {TailExpr, State2} = search(Tail, State1),
    {[HeadExpr | [TailExpr]], State2};
search({tail, Call = {call, _Line, _Fun, _Args}}, State) ->
     search(Call, State);
search({tail, Tail}, State) ->
    search(Tail, State);
search({record, _Line, _Name, ExprFields}, State) ->
    loop_with_state(ExprFields, fun expr_field/2, State);

search({record_index,_Line, _Name, Field}, State) ->
     search(Field, State);
search({record_field,_Line,Expression, _RecName, Field}, State) ->
    {ExprExpr, State1} = search(Expression, State),
    {FieldExpr, State2} = search(Field, State1),
    {[ExprExpr | [FieldExpr]], State2};

% How does this happen? (Foo).bar ?
%search({record_field,Line,Rec0,Field0}) ->
    %Rec1 = search(Rec0),
    %Field1 = search(Field0);
search(_IgnoredExpr, State) ->
    {[], State}.

expr_field({record_field, _Lf, {atom, _La, _F}, Expr}, State) ->
    search(Expr, State);
expr_field({record_field, _Lf, {var,_La,'_'}, Expr}, State) ->
    search(Expr, State).

guard_groups(GuardGroups, State) ->
    %map_separate(<<"; ">>, fun guard_group_conjunction/1, GuardGroups).
    {Exprs, State1} = loop_with_state(GuardGroups, fun guard_group_conjunction/2, State),
    {separate(<<"; ">>, Exprs), State1}.

guard_group_conjunction(GuardGroupConjunctionExpressions, State) ->
    Output = map_separate(<<", ">>, fun print/1, GuardGroupConjunctionExpressions),
    {Output, State}.

%% This is a list of generators _or_ filters
%% which are simply expressions
%% A generator is a target and a source
lc_bc_qual({generate,_Line,Target,Source}, State) ->
    %search(Target) ++ search(Source);

    %{TargetExpr, State1} = search(Target, State),
    %{SourceExpr, State2} = search(Source, State1),
    %{[TargetExpr | [SourceExpr]], State2};

    loop_with_state([Target, Source], fun search/2, State);

lc_bc_qual({b_generate,_Line,Target,Source}, State) ->
    %search(Target) ++ search(Source);
    loop_with_state([Target, Source], fun search/2, State);
lc_bc_qual(FilterExpression, State) ->
    search(FilterExpression, State).

%% Once we've found AST elements that we want in the CSV we need to convert them to text

print({var, _Line, VarName}) ->
    move_leading_underscore(a2b(VarName));

print({atom, _Line, Atom}) ->
    a2b(Atom);

print({integer, _Line, Int}) ->
    integer_to_binary(Int);

print({match, _Line, Var, Tuple}) ->
    [print(Var), <<" = ">>, print(Tuple)];

print({record, _Line, RecordName, Fields}) ->
    [<<"#">>, a2b(RecordName), <<"{">> | map_separate(fun print/1, Fields)] ++ [<<"}">>];

print({record_field, _Line, {atom, _Line2, FieldName}, {var, _VarLine, VarName}}) ->
    [a2b(FieldName), <<" = ">>, a2b(VarName)];

print({record_field, _Line, {atom, _Line2, FieldName}, {match, _Line3, Var, Record}}) ->
    [a2b(FieldName), <<" = ">>, print(Var), <<" = ">>, print(Record)];

print({record_field, _Line, {atom, _Line2, FieldName}, Call}) ->
    [a2b(FieldName), <<" = ">>, print(Call)];

print({tuple, _Line, Expressions}) ->
    [<<"{">> | map_separate(fun print/1, Expressions)] ++ [<<"}">>];

print({op, _Line, Operator, Expr1, Expr2}) ->
    [print(Expr1), <<" ">>, a2b(Operator), <<" ">>, print(Expr2)];

print({call, _Line, {atom, _Line2, FunctionName}, Params}) ->
    ParamBins = map_separate(<<", ">>, fun print/1, Params),
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

loop_with_state(Forms, Fun, State) ->
    lists:foldl(fun(Form, {Exps, StateInner}) ->
                    {Exp, StateAcc} = Fun(Form, StateInner),
                    {Exps ++ [Exp], StateAcc}
                end,
                _Acc = {[], State},
                Forms).
