# flat-maybe

**2024 UPDATE**: this repo is *unsafe* in the sense that it randomly segfaults under certain GHC versions and conditions that I have not investigated in detail. Use it at your own risk, but in most cases you should just not use it! I'm writing this note in 2024 because I see a few people starring the repo and I'd prefer them to not get random segfaults. Below is the README from 2015.

In the Rust programming language, there is a nice optimization for the `Option` (the analogue of `Maybe` in Haskell) type: if the `Option` holds a reference type, then the `Option<A>` values are represented as a pointer that's either some null-like value or a pointer to the object itself. In contrast, Haskell's `Maybe` always has at least two indirections:

    ptr    
     |
     |
    Just | ptr
           |
           |
          tag | data

This package implements the Rust scheme in an absolutely ungodly way. Basically, a `Maybe a` is either a pointer to an `a` object, or a pointer to a dummy "null" value that isn't exported from the library. The representation is `newtype Maybe a = Maybe Any`. We check "constructors" with `reallyUnsafePtrEquality#`. The result looks like this:

    ptr
     |
     |
     tag | data

One rather nasty thing about this solution is that our `Just` isn't parametrically polymorphic: `Just (Just Nothing)` immediately collapses to `Nothing` for any number of intermediate `Just`-s, but it works normally for any non-`Maybe` type. A funny consequence is that we can implement the monadic join as `unsafeCoerce`.  

Nasty this might be, the performance seems to be really good, probably the best I've seen for fast error handling. 
