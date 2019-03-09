
{-# LANGUAGE CPP, TypeOperators, DataKinds, PolyKinds, TypeFamilies #-}

-- | A collection of type-level operators.
module Control.Type.Operator ( type (^>), type (<^), type ($), type (&)
                             , type ($$), type (<+>), type (<=>)
                             ) where


#if __GLASGOW_HASKELL__ <= 710
import GHC.Prim (Constraint)
#else
import Data.Kind (Constraint, Type)
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

{-# DEPRECATED (<=>) "Since (\\<+\\>) is now kind-polymorphic and accepts the arguments on either side (\\<=\\>) will be removed in a future version." #-}

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

-- | Map any constraints over any type variables.
--
-- @
-- a :: [Show, Read] \<+> a => a -> a
-- =
-- a :: (Show a, Read a) => a -> a
--
-- a :: Show \<+> [a, b, c] => a -> b -> c -> String
-- =
-- a :: (Show a, Show b, Show c) => a -> b -> c -> String
-- @
type family (<+>) (a :: k1) (b :: k2) :: Constraint
type instance (<+>) _ [] = (() :: Constraint)
type instance (<+>) [] _ = (() :: Constraint)
type instance (<+>) (c ': cs) (a :: Type) = (c a, a <+> cs)
type instance (<+>) c (a ': as) = (c a, c <+> as)
infixl 9 <+>

