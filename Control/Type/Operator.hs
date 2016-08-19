
{-# LANGUAGE TypeOperators, LiberalTypeSynonyms, RankNTypes,
             ConstraintKinds, KindSignatures, TypeFamilies,
             PolyKinds, DataKinds, CPP, MultiParamTypeClasses,
             UndecidableSuperClasses, FlexibleInstances #-}

-- | A collection of type-level operators.
module Control.Type.Operator ( type (^>), type (<^), type ($), type (&)
                             , type ($$), type (<+>), type (<=>)
                             ) where


#if __GLASGOW_HASKELL__ <= 710
import GHC.Prim (Constraint)
#else
import Data.Kind (Constraint)
#endif


-- | Empty typeclass constraint.
class Empty a
instance Empty a


-- | A tightly binding version of @->@ that lets you strip parentheses from
-- function types in certain spots. Example:
--
-- @
-- f :: Maybe Int ^> String
-- =
-- f :: Maybe (Int -> Int)
-- @
type (^>) = (->)
infixr 5 ^>

-- | A flipped '^>'.
--
-- @
-- f :: Maybe String <^ Int
-- @
--
-- Note: this is not partially applied like '^>' and @->@.
type (<^) a b = (^>) b a
infixr 5 <^

-- | Infix application.
--
-- @
-- f :: Either String $ Maybe Int
-- =
-- f :: Either String (Maybe Int)
-- @
type f $ a = f a
infixr 2 $

-- | A flipped '$'.
--
-- @
-- f :: Maybe Int & Maybe
-- =
-- f :: Maybe (Maybe Int)
-- @
type a & f = f a
infixl 1 &

-- | Infix application that takes a two arguments rather than just one.
--
-- @
-- f :: Either $$ Int ^> Int $ Int
-- =
-- f :: Either (Int -> Int) Int
-- @
type (f $$ a) b = f a b
infixr 3 $$

-- | Map a constraint over several variables.
--
-- @
-- a :: Show \<=> [a, b] => a -> b -> String
-- =
-- a :: (Show a, Show b) => a -> b -> String
-- @
type family (<=>) (c :: k -> Constraint) (as :: [k]) where
    (<=>) c '[k] = c k
    (<=>) c (h ': t) = (c h, (<=>) c t)
infixl 9 <=>

-- | Map several constraints over a single variable.
--
-- @
-- a :: [Show, Read] \<+> a => a -> a
-- =
-- a :: (Show a, Read a) => a -> a
-- @
type family (<+>) (c :: [k -> Constraint]) (a :: k) where
    (<+>) '[] a = Empty a
    (<+>) '[c] a = c a
    (<+>) (ch ': ct) a = (ch a, (<+>) ct a)
infixl 9 <+>

