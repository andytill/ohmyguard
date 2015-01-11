# ohmyguard

Parse transform for converting `my_func(X/binary) -> X.` to `my_func(X) when is_binary(X) -> X`.

## Why?

Erlangs binary pattern matching is one of its best features and its most succinct syntax, so why not apply it to function guards as well?

ohmyguard will not completely replace traditional guards, it is just for type checking.

## Should I use it?

No way!  It is just a proof of concept at the moment.