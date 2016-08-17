
{-# LANGUAGE TypeOperators, LiberalTypeSynonyms, RankNTypes,
             ConstraintKinds, KindSignatures, TypeFamilies,
             ImpredicativeTypes, PolyKinds, DataKinds #-}

-- | A collection of type-level operators.
module Control.Type.Operator where


import Data.Kind


-- | A tightly binding version of @->@ that lets you strip parentheses from
-- function types in certain spots. Example:
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

-- | Map a constraint over several variables.
--
-- >>> a :: Cmap Show [a, b] => a -> b -> String
-- >>> =
-- >>> a :: (Show a, Show b) => a -> b -> String
type family Cmap (c :: k -> Constraint) (ls :: [k]) where
    Cmap c '[k] = c k
    Cmap c (h ': t) = (c h, Cmap c t)

-- | Infix 'Cmap'.
--
-- >>> a :: Show <=> [a, b] => a -> b -> String
-- >>> =
-- >>> a :: (Show a, Show b) => a -> b -> String
type (c <=> ls) = Cmap c ls
infixl 9 <=>

-- | Map several constraints over a single variable.
--
-- >>> a :: Cfor [Show, Read] a => a -> a
-- >>> =
-- >>> a :: (Num a, Show a, Read a) => a -> IO ()
type family Cfor (c :: [k -> Constraint]) (a :: k) where
    Cfor '[c] a = c a
    Cfor (ch ': ct) a = (ch a, Cfor ct a)

-- | Infix 'Cfor'.
--
-- >>> a :: [Show, Read] <+> a => a -> a
-- >>> =
-- >>> a :: (Num a, Show a, Read a) => a -> IO ()
type (cs <+> a) = Cfor cs a
infixl 9 <+>


