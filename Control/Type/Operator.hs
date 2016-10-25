
{-# LANGUAGE TypeOperators, LiberalTypeSynonyms, RankNTypes,
             ConstraintKinds, KindSignatures, TypeFamilies,
             PolyKinds, DataKinds, CPP, MultiParamTypeClasses,
             FlexibleInstances #-}

-- | A collection of type-level operators.
module Control.Type.Operator ( type (^>), type (<^), type ($), type (&)
                             , type ($$), type (<+>), type (<=>)
                             ) where


#if __GLASGOW_HASKELL__ <= 710
import GHC.Prim (Constraint)
#else
import Data.Kind (Constraint)
#endif


-- | A tightly binding version of @->@ that lets you strip parentheses from
-- function types in certain spots. Example:
--
-- @
-- f :: Maybe Int ^> String
-- =
-- f :: Maybe (Int -> String)
-- @
type (^>) = (->)
infixr 5 ^>

-- | A flipped '^>'.
--
-- @
-- f :: Maybe String <^ Int
-- =
-- f :: Maybe (Int -> String)
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

-- | Infix application that can take two arguments in combination with '$'.
--
-- @
-- f :: Either $$ Int ^> Int $ Int ^> Int
-- =
-- f :: Either (Int -> Int) (Int -> Int)
-- @
type (f $$ a) = f a
infixr 3 $$

-- | Map a constraint over several variables.
--
-- @
-- a :: Show \<=> [a, b] => a -> b -> String
-- =
-- a :: (Show a, Show b) => a -> b -> String
-- @
type family (<=>) (c :: k -> Constraint) (as :: [k]) where
    (<=>) c '[] = (() :: Constraint)
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
    (<+>) '[] a = (() :: Constraint)
    (<+>) (ch ': ct) a = (ch a, (<+>) ct a)
infixl 9 <+>

-- | Tuple constructor operator
--
-- @
-- a :: String * Int * (Int, Int)
-- =
-- a :: (String, Int, (Int, Int))
-- @
--
-- Note: this will flatten tuples on the left-hand-side.
--
-- @
-- a :: (a, b) * c
-- =
-- a :: (a, b, c)
-- @
type family (*) f1 f2 where
    (*) (a, b, c, d, e, f, g, h) i = (a, b, c, d, e, f, g, h, i)
    (*) (a, b, c, d, e, f, g)    h = (a, b, c, d, e, f, g, h)
    (*) (a, b, c, d, e, f)       g = (a, b, c, d, e, f, g)
    (*) (a, b, c, d, e)          f = (a, b, c, d, e, f)
    (*) (a, b, c, d)             e = (a, b, c, d, e)
    (*) (a, b, c)                d = (a, b, c, d)
    (*) (a, b)                   c = (a, b, c)
    (*) a                        b = (a, b)
infixl 9 *

