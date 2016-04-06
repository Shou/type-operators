
{-# LANGUAGE TypeOperators, LiberalTypeSynonyms, RankNTypes,
             ConstraintKinds, KindSignatures #-}

-- | A collection of type-level operators.
module Control.Type.Operator
    where


import GHC.Prim (Constraint)


-- | A tightly binding version of @->@ that lets you strip parentheses from
-- first-class type-functions. Example:
--
-- >>> f :: Maybe Int ^> String
-- f :: Maybe (Int -> Int)
type (^>) = (->)
infixr 5 ^>

-- | A flipped '^>'.
--
-- >>> f :: Maybe String <^ Int
--
-- Note: this is not partially applied like '^>' and @->@.
type (<^) a b = (^>) b a

-- | Infix application.
--
-- >>> f :: Either String $ Maybe Int
-- f :: Either String (Maybe Int)
type f $ a = f a
infixr 2 $

-- | A flipped '$'.
--
-- >>> f :: Maybe Int & Maybe
-- f :: Maybe (Maybe Int)
type a & f = f a
infixl 1 &

-- | Infix application that takes a two arguments rather than just one.
--
-- >>> f :: Either $$ Int ^> Int $ Int
-- f :: Either (Int -> Int) Int
type (f $$ a) b = f a b
infixr 3 $$

-- | Syntactic sugar for type constraints, allowing omission of repeat
-- type variables.
--
-- >>> a :: (Num % Show) a => a -> String
-- a :: (Num a, Show a) => a -> String
type (%) (c1 :: * -> Constraint) (c2 :: * -> Constraint) a = (c1 a, c2 a)
infixr 2 %

