{-# LANGUAGE GeneralizedNewtypeDeriving #-} --language extensions
module Monild where

newtype Sum' a = Sum' a
    deriving (Eq, Ord, Num, Show)

instance Num a => Semigroup (Sum' a) where
    (Sum' x) <> (Sum' y) = Sum' (x + y)

-- Semigroup is superclass of Monoid
instance Num a => Monoid (Sum' a) where
    mempty = Sum' 0

newtype Product a = Product a
    deriving (Eq, Ord, Num, Show)

instance Num a => Semigroup (Product a) where
    (Product x) <> (Product y) = Product (x * y)

instance Num a => Monoid (Product a) where
    mempty = Product 1

-- Minimum cant be made for "Integer" because of we cant define "mempty" for it.
newtype Min = Min Int
    deriving (Eq, Ord, Num, Show)

instance Semigroup Min where 
    (Min x) <> (Min y) = Min (min x y)

instance Monoid Min where
    mempty = Min (minBound)  
