# ohmyguard

Parse transform for converting `my_func(X/binary) -> X.` to `my_func(X) when is_binary(X) -> X`.

## Why?

Erlang binary pattern matching is one of its best features and its most succinct syntax, so why not apply it to function guards as well?  Given this function:

```erlang
my_func(Value) when is_binary(Value) ->
    Value.
```

It can be reduced to this:

```erlang
my_func(Value/binary) ->
    Value.
```

**ohmyguard** supports all erlang types:

```erlang
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
```

The syntax proves even more concise when a function has multiple arguments that need to be checked:

```erlang
omg_pid_atom(X/pid, Y/atom) -> {X, Y}.
```

The `andalso` and `orelse` keywords and confusing commas and semi colons are gone and the function is more readable.  All type guards must be met or a `function_clause` error is thrown.

Records are also supported, although the syntax is still being worked on:

```erlang
-record(myrec, {field = value}).

omg_record(Rec/#myrec{}) -> Rec.
```

It would be better if record guards looked like `f(Rec/#myrec) -> Rec.` but this throws a parse error before it can be given to the **ohmyguard** parse transform.  Suggestions welcome!

**ohmyguard** will not completely replace traditional guards, it is just for type checking.

## Syntax that still needs to be implemented...

##### Tuples in function clauses

```erlang
omg_pid_atom({X/pid, Y/atom}) -> 
    {X, Y}.
```

##### Lists in function clauses

```erlang
omg_list([H/integer | T]) -> 
    ok.
```

##### Case statements

```erlang
case V of
	{ok, V/binary} -> V
end
```

##### Assignments

```erlang
{ok, V/binary} = application:get_env(my_prop)
```

##### And much more...

It would be nice if **ohmyguard** had integration with the [recless](http://code.google.com/p/recless/) parse transform, to allow syntax like:

```erlang
address_street(Address/#address) -> 
	Address.street.
```

Also funs, list comprehensions, the mind boggles.

## Should I use it?

No way!  It is just a proof of concept at the moment.
