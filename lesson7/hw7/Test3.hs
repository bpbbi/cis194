module Test3 where

class Sized a where
    size :: a -> Int

data MyData = Md [Int]

instance Sized MyData where 
    size (Md xs) = length xs

l = Md [1,2,3,5,6]
