-module(ohmyguard).

-export([parse_transform/2]).




%% Convert this jazz...
%%
%% [{attribute,1,file,{"omg_simplest.erl",1}},
%%  {attribute,1,module,omg_simplest},
%%  {attribute,5,export,[{omg_1,1}]},
%%  {function,7,omg_1,1,
%%            [{clause,7,
%%                     [{op,7,'/',{var,7,'X'},{atom,7,binary}}],
%%                     [],
%%                     [{atom,7,ok}]}]},
%%  {eof,7}]
%%
%% To this...
%%
%% [{attribute,1,file,{"omg_simplest.erl",1}},
%%  {attribute,1,module,omg_simplest},
%%  {attribute,5,export,[{omg_1,1}]},
%%  {function,7,omg_1,1,
%%            [{clause,7,
%%                     [{var,7,'X'}],
%%                     [[{call,7,{atom,7,is_binary},[{var,7,'X'}]}]],
%%                     [{atom,7,ok}]}]},
%%  {eof,7}]

parse_transform(AST_in, _Options) -> 
%   io:format("== AST ==~n~p~n== AST ==~n~n", [AST_in]),

    AST_in_1 = [ast(A) || A <- AST_in],
    
%   io:format("~n== New AST ==~n~p~n== New AST ==~n~n", [AST_in_1]),

    AST_in_1.

ast({function, _, _, _, Clauses_0} = Func_AST) ->
    Clauses_1 = [transform_clause(C) || C <- Clauses_0],
    setelement(5, Func_AST, Clauses_1);
ast(Other_AST) ->
    Other_AST.

transform_clause({clause, N, Args, Guards, Body} = _Clause) ->
    {Args_1, Types} = lists:mapfoldl(fun transform_args/2, [], Args),
    Types_Guards = [type_guard(N, T) || T <- Types],
    {clause, N, Args_1, [Types_Guards | Guards], Body}.

transform_args({var, _, _} = Var, Acc) ->
    {Var, Acc};
transform_args({op, N, '/', {var, N, Var_name} = Arg, {atom, N, Var_type}}, Acc) ->
    {Arg, [{Var_name, Var_type} | Acc]}.

type_guard(N, {Name, Type}) ->
    {call, N, {atom, N, type_to_guard(Type)}, [{var, N, Name}]}.

type_to_guard(binary) -> is_binary.