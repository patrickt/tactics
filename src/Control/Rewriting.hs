{-# LANGUAGE DerivingVia, FlexibleContexts, FlexibleInstances, GADTs, LambdaCase, StandaloneDeriving, TypeOperators #-}

-- | This module provides 'Rewrite', a monadic DSL that abstracts the
-- details of rewriting a given datum into another type, supporting
-- failure and retries. A set of /strategic combinators/, as described
-- in Lämmel et al.'s /The Essence of Strategic Programming/, is
-- provided to promote single-layer rewrites into top-down or
-- bottom-up tree transformations.
module Control.Rewriting
  ( -- | Core types
    Rewrite
  , Rule
  -- | Combinators
  , target
  , purely
  , ensure
  , try
  , (>+>)
  -- | Reexports from Control.Category
  , (>>>)
  , (<<<)
  , (^>>)
  -- | Predicate filtering
  , refine
  , only
  -- | Useful rewrites
  , mhead
  , mjust
  -- | Running rewrites
  , rewrite
  , recursively
  -- | Strategic rewrite combinators
  , applyAll
  , applyAny
  , topDownAll
  , topDownAny
  ) where

import Prelude hiding (id, (.))

import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Monad
import           Control.Selective
import           Data.Coerce
import           Data.Functor.Foldable
import qualified Data.Functor.Foldable as Foldable
import           Data.Maybe
import           Data.Monoid

-- | The fundamental type of rewriting rules. A @Rewrite t a@ maps
-- values of type @t@ to values of type @a@, supporting failure and choice.
-- If you need to layer monadic effects on top of a 'Rewrite', use a 'Lift'
-- effect.
data Rewrite t a where
  -- TODO: Choice is inflexible and slow. A Sum over fs can be queried for its index, and we can build a jump table over that.
  -- We can copy NonDet to have fair conjunction or disjunction.
  Choice :: Rewrite t a -> Rewrite t a -> Rewrite t a
  Target :: Rewrite t t
  Empty  :: Rewrite t a
  Comp   :: Rewrite b c -> Rewrite a b -> Rewrite a c
  Split  :: Rewrite b c -> Rewrite b' c' -> Rewrite (b, b') (c, c')
  -- We could have implemented this by changing the semantics of how Then is interpreted, but that would make Then and Sequence inconsistent.
  Match  :: (t -> Maybe u) -> Rewrite u a -> Rewrite t a
  Then   :: Rewrite t b -> (b -> Rewrite t a) -> Rewrite t a

-- | A 'Rule' is a 'Rewrite' with identical input and output types.
type Rule t = Rewrite t t

deriving via (WrappedArrow Rewrite t) instance Functor (Rewrite t)
deriving via (WrappedArrow Rewrite t) instance Applicative (Rewrite t)
deriving via (WrappedArrow Rewrite t) instance Alternative (Rewrite t)
deriving via (SelectA (Rewrite t))    instance Selective (Rewrite t)

instance Monad (Rewrite t) where
  (>>=) = Then

-- | Rewrites are generally composed left-to-right with '>>>'.
instance Category Rewrite where
  id  = Target
  (.) = Comp

instance Arrow Rewrite where
  (***) = Split
  arr f = fmap f target

instance ArrowZero Rewrite where
  zeroArrow = Empty

instance ArrowPlus Rewrite where
  (<+>) = (<|>)

instance Semigroup (Rewrite t t) where
  (<>) = (<|>)

instance Monoid (Rewrite t t) where
  mempty = id

-- | 'target' extracts the 't' that a given 'Rewrite' is operating upon.
-- Similar to a reader monad's 'ask' function. This is an alias for 'id'
-- and 'mempty'.
target :: Rewrite t t
target = id

-- | 'ensure' succeeds iff the provided predicate function returns
-- true when applied to the rewrite's 'target'. If it succeeds, it
-- returns the target.
ensure :: (t -> Bool) -> Rewrite t t
ensure f = target >>= \c -> c <$ guard (f c)

-- | Promote a pure function to a 'Rewrite'. An alias for 'arr'.
purely :: (a -> b) -> Rewrite a b
purely = arr

-- | 'refine' takes a modification function and a new 'Rewrite', the
-- input parameter of which is the result of the modification
-- function. If the modification function returns 'Just' when applied
-- to the current 'target', the given rewrite action is executed with
-- the result of that 'Just' as the new target; if 'Nothing' is
-- returned, the action fails.
--
-- This is a low-level combinator useful for matching over
-- non-recursive types. When dealing with recursive types such as
-- 'Term', you'll generally use the 'enter' and 'narrow' combinators.
refine :: (t -> Maybe u) -> Rewrite u a -> Rewrite t a
refine = Match

-- | An alias for the common pattern of @match f id@.
only :: (t -> Maybe u) -> Rewrite t u
only f = Match f Target

-- | @a >+> b@ performs @a@ and @b@ in sequence, succeeding if one
-- or both succeed.
infixr 1 >+>
(>+>) :: Rule t -> Rule t -> Rule t
a >+> b = (a >>> try b) <|> b

-- | Attempt to run a rule, falling back on the identity rule if it fails.
try :: Rule a -> Rule a
try a = a <|> id

-- | Matches the head of the input list. Fails if the list is empty.
--
-- @mhead = only listToMaybe@
mhead :: Rewrite [a] a
mhead = only listToMaybe

-- | Matches 'Just' values.
--
-- @mjust = only id@
mjust :: Rewrite (Maybe a) a
mjust = only id

-- | Run one step of a 'Rewrite' computation. Look at 'recursively' if you want something
-- that folds over subterms.
rewrite :: (Alternative m, Monad m) => t -> Rewrite t a -> m a
rewrite t = \case
  Choice a b -> rewrite t a <|> rewrite t b
  Target     -> pure t
  Match f m  -> foldMapA (`rewrite` m) (f t)
  Comp g f   -> rewrite t f >>= (`rewrite` g)
  Empty      -> empty
  Then m f   -> rewrite t m >>= rewrite t . f
  Split f g  -> pure t >>= \(a, b) -> (,) <$> rewrite a f <*> rewrite b g

-- | Run a 'Rewrite' over a 'Recursive' data structure, leaf-to-root. Unlike recursion with
-- the Lämmel combinators, this allows you to use a 'Rewrite' rather than just a 'Rule',
-- allowing for the returned type to change. If you pass in a 'Rule' here, consider whether
-- the strategic combinators are actually what you want.
recursively :: (Alternative m, Monad m, Corecursive t, Recursive t, Foldable (Base t))
            => Rewrite t a
            -> t
            -> m a
recursively m = para (paraRewrite m)

type RAlgebra f t a = f (t, a) -> a

paraRewrite :: (Alternative m, Monad m, Corecursive t, Foldable (Base t))
            => Rewrite t a
            -> RAlgebra (Base t) t (m a)
paraRewrite m t = rewrite (embedTerm t) m <|> foldMapA snd t

-- * For more information about these combinators, see /The Essence of Strategic Programming/
-- (Lämmel, Visser, and Visser, 2003).

-- | Runs a 'Rule' one level deep in the provided 'Recursive' type, succeeding
-- if all children succeed.
applyAll :: (Corecursive t, Recursive t, Traversable (Base t))
         => Rule t -> Rule t
applyAll m = id >>= \t -> Foldable.embed <$> traverse go (Foldable.project t)
  where go a = pure a >>> m

-- | Runs a 'Rule' one level deep, succeeding if any children succeed.
applyAny :: (Corecursive t, Recursive t, Traversable (Base t))
         => Rule t -> Rule t
applyAny m = applyAll (m <|> id)

-- | Runs a 'Rule' top-down over a 'Recursive' type, succeeding if
-- all subsequent rewrites succeed.
topDownAll :: (Corecursive t, Recursive t, Traversable (Base t))
           => Rule t -> Rule t
topDownAll r = let go = r >>> applyAll go in go

-- | Runs a 'Rule' top-down over a 'Recursive' type, succeeding if
-- any rewrites succeed.
topDownAny :: (Corecursive t, Recursive t, Traversable (Base t))
           => Rule t -> Rule t
topDownAny r = let go = r >+> applyAny go in go

embedTerm :: Corecursive t => Base t (t, a) -> t
embedTerm e = embed (fst <$> e)

foldMapA :: (Foldable t, Alternative m) => (a -> m b) -> t a -> m b
foldMapA f = getAlt #. foldMap (Alt #. f)

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
