# tactics

[![Hackage](https://img.shields.io/hackage/v/tactics.svg)](https://hackage.haskell.org/package/tactics)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A library for strategic rewriting. Rewrite rules look like this:

```haskell
process :: Rewrite User Record
increment :: Rule Int -- aka Rewrite Int Int
```

They are written with a monadic syntax, or the arrow syntax if you so choose. There is also a complete library of useful combinators; if your data structure can't conveniently be crawled with this library, please file an issue—that's a bug!

You can invoke a rule with `rewrite`. The semantics of this rule (choice, failure, and sequencing) are inferred from the provided `Alternative`-implementing `Monad`. For example, specializing `m` to `Maybe` returns the first successful result, whereas the `[]` monad returns all successful results.

```
rewrite :: (Alternative m, Monad m) => t -> Rewrite t a -> m a
```

If you use the `recursion-schemes` library and provide `Recursive` and `Corecursive` instances for your data type, you can apply a `Rewrite` at every level of a data structure:

``` haskell
recursively :: (Alternative m, Monad m, Corecursive t, Recursive t, Foldable (Base t))
            => Rewrite t a
            -> t
            -> m a
```

There also exists directed rewrite rules, in the manner of Lämmel et al.'s _The Essence of Strategic Programming_.

``` haskell
applyAll :: (Corecursive t, Recursive t, Traversable (Base t))
         => Rule t -> Rule t
```

This library was extracted from the [semantic](https://github.com/github/semantic) project, and as such is licensed under the MIT license.
