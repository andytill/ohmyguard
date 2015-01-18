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
%%
%%
%% [{attribute,1,file,{"test/omg_simplest.erl",1}},
%% {attribute,1,module,omg_simplest},
%% {attribute,1,file,{"/usr/lib/erlang/lib/eunit-2.2.7/include/eunit.hrl",1}},
%% {attribute,6,file,{"test/omg_simplest.erl",6}},
%% {attribute,11,record,
%%            {myrec,[{record_field,11,{atom,11,field},{atom,11,value}}]}},
%% {function,13,omg_record,1,
%%           [{clause,13,
%%                    [{var,13,'Rec'}],
%%                    [[{call,13, {atom,13,is_record}, [{var,13,'Rec'},{atom,13,myrec}]}]],
%%                    [{var,13,'Rec'}]}]},
%% {eof,111}]



parse_transform(AST_in, _Options) -> 
    % io:format("== AST ==~n~p~n== AST ==~n~n", [AST_in]),

    AST_out = [ast(A) || A <- AST_in],
    
    % io:format("~n== New AST ==~n~p~n== New AST ==~n~n", [AST_out]),

    AST_out.

ast({function, _, _, _, Clauses_0} = Func_AST) ->
    Clauses_1 = [transform_clause(C) || C <- Clauses_0],
    setelement(5, Func_AST, Clauses_1);
ast(Other_AST) ->
    Other_AST.

transform_clause({clause, N, Args, Guards_0, Body}) ->
    {Args_1, Types} = lists:mapfoldl(fun transform_args/2, [], Args),
    Types_Guards = [type_guard(N, T) || T <- Types],
    Guards_1 =
        case Types_Guards of
            [] -> Guards_0;
            _  -> [Types_Guards | Guards_0]
        end,
    {clause, N, Args_1, Guards_1, Body}.

%% take the AST containing the `V/type' syntax and return regular AST for 
%% arguments and a list of all guards that need to be created.
transform_args({var, _, _} = Var, Acc) ->
    {Var, Acc};
transform_args({cons, N, Head, Tail}, Acc) ->
    %% handle the head element of lists
    {Head_1, Acc_1} = transform_args(Head, Acc),

    % we could easily just pass the tail into transform args, but I don't want
    % nil to leak out into general arg checking since it is specific to lists
    {Tail_1, Acc_2} = 
        case Tail of
            {nil, _} = Nil ->
                {Nil, Acc_1};
            _ ->
                transform_args(Tail, Acc_1)
        end,
    Cons = {cons, N, Head_1, Tail_1},
    {Cons, Acc_2};
transform_args({tuple, N, Elements}, Acc) ->
    {Elements_1, Acc_1} = lists:mapfoldl(fun transform_args/2, Acc, Elements),
    {{tuple, N, Elements_1}, Acc_1};
transform_args({op, N, '/', {var, N, Var_name} = Arg, {atom, N, Var_type}}, Acc) ->
    {Arg, [{Var_name, Var_type} | Acc]};
transform_args({op, N, '/', {var, N, Var_name} = Arg, {record,N,Record_name,[]}}, Acc) ->
    {Arg, [{Var_name, Record_name} | Acc]};
transform_args(Match, Acc) when element(1, Match) == match ->
    % a match looks like myfunc(#myrec{} = Rec) -> ok.
    {Match, Acc}.

type_guard(N, {Var_name, Type}) ->
    case maps:find(Type, type_to_guards()) of
        {ok, Guard} -> primitive_guard(N, Var_name, Guard);
        error       -> record_guard(N, Var_name, Type) 
    end.

record_guard(N, Var_name, Record_name)
        when
            is_integer(N),
            is_atom(Var_name),
            is_atom(Record_name) ->
    {call, N, {atom,N,is_record}, [{var,N,Var_name}, {atom,N,Record_name}]}.

primitive_guard(N, Name, Guard)
        when
            is_integer(N),
            is_atom(Name),
            is_atom(Guard) ->
    {call, N, {atom,N,Guard}, [{var,N,Name}]}.

%% all the types that are allowed after the the slash as a guard.
type_to_guards() ->
    #{ atom => is_atom,
       binary => is_binary,
       bitstring => is_bitstring,
       float => is_float,
       function => is_function,
       integer => is_integer,
       list => is_list,
       map => is_map,
       number => is_number,
       pid => is_pid,
       port => is_port,
       reference => is_reference,
       ref => is_reference,
       tuple => is_tuple }.