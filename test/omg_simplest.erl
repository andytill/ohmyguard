-module(omg_simplest).

-compile({parse_transform, ohmyguard}).

-export([omg_binary/1]).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================%%
%%
%% =============================================================================

omg_atom(X/atom) -> X.
omg_binary(X/binary) -> X.
omg_bitstring(X/bitstring) -> X.
omg_float(X/float) -> X.
omg_function(X/function) -> X.
omg_integer(X/integer) -> X.
omg_list(X/list) -> X.
omg_map(X/map) -> X.
omg_number(X/number) -> X.
omg_pid(X/pid) -> X.
omg_port(X/port) -> X.
omg_reference(X/reference) -> X.
omg_ref(X/ref) -> X.
omg_tuple(X/tuple) -> X.

omg_atom_atom(X/atom, Y/atom) -> {X, Y}.

% TODO this is probably being checked twice
omg_atom_guard(X/atom) when is_atom(X) -> X.

omg_pid_number(X/pid, Y/atom) -> {X, Y}.


%% =============================================================================
%% tests
%% =============================================================================

omg_atom_test()      -> ?assertEqual(derp, omg_atom(derp)).
omg_binary_test()    -> ?assertEqual(<<"hello">>, omg_binary(<<"hello">>)).
omg_bitstring_test() -> ?assertEqual(<<4:1,4:6>>, omg_bitstring(<<4:1,4:6>>)).
omg_float_test()     -> ?assertEqual(3.5, omg_float(3.5)).
omg_function_test()  -> F = fun() -> ok end,
                        ?assertEqual(F, omg_function(F)).
omg_integer_test()   -> ?assertEqual(8, omg_integer(8)).
omg_list_test()      -> ?assertEqual([1,3,4], omg_list([1,3,4])).
omg_map_test()       -> ?assertEqual(#{ n => 2 }, omg_map(#{ n => 2 })).
omg_number_test()    -> ?assertEqual(2, omg_number(2)).
omg_pid_test()       -> P = spawn(fun() -> ok end),
                        ?assertEqual(P, omg_pid(P)).
% omg_port_test()      -> ?assertEqual(derp, omg_port(derp)). TODO
omg_reference_test() -> Ref = make_ref(),
                        ?assertEqual(Ref, omg_reference(Ref)).
omg_ref_test()       -> Ref = make_ref(),
                        ?assertEqual(Ref, omg_ref(Ref)).
omg_tuple_test()     -> ?assertEqual({}, omg_tuple({})).

omg_not_atom_test()      -> ?assertException(error, function_clause, omg_atom(1)).
omg_not_binary_test()    -> ?assertException(error, function_clause, omg_binary(1)).
omg_not_bitstring_test() -> ?assertException(error, function_clause, omg_bitstring(1)).
omg_not_float_test()     -> ?assertException(error, function_clause, omg_float(1)).
omg_not_function_test()  -> ?assertException(error, function_clause, omg_function(1)).
omg_not_integer_test()   -> ?assertException(error, function_clause, omg_integer(derp)).
omg_not_list_test()      -> ?assertException(error, function_clause, omg_list(1)).
omg_not_map_test()       -> ?assertException(error, function_clause, omg_map(1)).
omg_not_number_test()    -> ?assertException(error, function_clause, omg_number(derp)).
omg_not_pid_test()       -> ?assertException(error, function_clause, omg_pid(1)).
omg_not_port_test()      -> ?assertException(error, function_clause, omg_port(1)).
omg_not_reference_test() -> ?assertException(error, function_clause, omg_reference(1)).
omg_not_ref_test()       -> ?assertException(error, function_clause, omg_ref(1)).
omg_not_tuple_test()     -> ?assertException(error, function_clause, omg_tuple(1)).

omg_atom_atom_test()       -> ?assertEqual({derp, troll}, omg_atom_atom(derp, troll)).
omg_not_atom_atom_1_test() -> ?assertException(error, function_clause, omg_atom_atom(1, troll)).
omg_not_atom_atom_2_test() -> ?assertException(error, function_clause, omg_atom_atom(derp, 1)).
omg_not_atom_atom_3_test() -> ?assertException(error, function_clause, omg_atom_atom(1, 1)).

omg_atom_guard_test() -> ?assertEqual(derp, omg_atom_guard(derp)).

omg_pid_number_test() -> 
    P = spawn(fun() -> ok end),
    ?assertEqual({P, derp}, omg_pid_number(P, derp)).
