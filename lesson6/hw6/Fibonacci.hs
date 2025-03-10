module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib1 :: [Integer]
fib1 = map fib [0..] 

fib2 :: [Integer]
fib2 = fibs 0 1
    where fibs a b = a : fibs b (a + b)

data Stream a = Cons a (Stream a) 
     deriving (Eq) 

fib3 :: (Stream Integer)
fib3 = fibs 0 1
    where fibs a b = (Cons a (fibs b (a + b)))

instance Show a => Show (Stream a) where
    show stream = showi stream 20
        where showi (Cons x xs) i 
                                | i > 0 = show x ++ " " ++ (showi xs (i - 1))
                                | otherwise = show x 

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)  

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs) 

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Cons n $ streamFromSeed f (f n)

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

ruler :: Stream Integer
ruler = streamMap (fun) nats
        where fun = toInteger . length . filter (\x -> (mod x 2) == 0) . takeWhile (>1) . iterate (\x -> if mod x 2 == 0 then div x 2 else 0) 

