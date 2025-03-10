module Test4 where

import Sized

newtype MyArr = Ma Int deriving Show

ss :: (MyArr, Size)
ss = (Ma 2, Size 5)
