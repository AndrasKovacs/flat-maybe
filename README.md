# flat-maybe
Rust-style strict Maybe in Haskell: no space/indirection overhead.

It's implemented in a really ungodly way, but at least it's much faster then Maybe (detailed benchmarks pending!). Use it at your own risk.

Basically, a `Maybe a` is either a pointer to an `a` object, or a pointer to a dummy "null" value that isn't exported from the library. We check "constructors" with `reallyUnsafePtrEquality#`. 

One rather nasty thing about this solution is that our `Just` isn't parametrically polymorphic: `Just (Just Nothing)` immediately collapses to `Nothing` for any number of intermediate `Just`-s, but it works normally for any non-`Maybe` type. 
