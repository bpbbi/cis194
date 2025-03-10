module Fff where 

import Prelude hiding (foldr, foldl)

veryBigList = [1..1000000]

foldr f z [] = z
foldr f z (x:xs) = x `f` foldr z xs

sum1 = foldr (+) 0

