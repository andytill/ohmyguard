# ohmyguard

Parse transform for converting `my_func(X/binary) -> X.` to `my_func(X) when is_binary(X) -> X`.

## Why?

Erlangs binary pattern matching is one of its best features and its most succinct syntax, so why not apply it to function guards as well?  Given this function:

```erlang
my_func(Value) when is_binary(Value) ->
    ok.
```

It can be reduced to this:

```erlang
my_func(Value/binary) ->
    ok.
```

IMO it is much easier to read and has a shorter line length.  ohmyguard will not completely replace traditional guards, it is just for type checking.

## Should I use it?

No way!  It is just a proof of concept at the moment.
