module SieveOfEratosthenes where
import Data.Function
--s = fix (\f (x:xs) -> x : filter (\y -> mod y x /= 0) (f xs)) [2..]
sieve :: [Int] -> [Int]
sieve = (\(x:xs) -> x : filter (\y -> mod y x /= 0) (sieve xs))

main :: IO()
main = do
    print "Enter limit:"
    input <- getLine
    let limit = read input :: Int
    print $ take limit $ sieve [2..]
