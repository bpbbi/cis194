module Hw1 where

toDigitsRev :: Int -> [Int]
toDigitsRev x
    | x <= 0 = [] 
    | x > 0 = mod x 10 : (toDigitsRev $ div x 10)

toDigits :: Int -> [Int]
toDigits x = reverse $ toDigitsRev x

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : y * 2 : doubleEveryOther zs

sumDigits :: [Int] -> Int
sumDigits xs = sum $ xs >>= toDigits

validate :: Int -> Bool
validate x = mod (sumDigits $ doubleEveryOther $ toDigitsRev x) 10 == 0

solve :: IO()
solve = do 
    putStrLn $ "validate 4012888888881881 " ++ if validate 4012888888881881 then "true" else "false" -- expect true
    putStrLn $ "validate 4012888888881882 " ++ if validate 4012888888881882 then "true" else "false" -- expect false