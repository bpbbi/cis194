module Main where

xor :: [Bool] -> Bool
xor = foldr (xorimpl) False 

xorimpl:: Bool -> Bool -> Bool 
xorimpl cur prev = if not cur then prev else not prev

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f xs = foldr (\x -> (++) $ (f x) : []) [] xs 

main :: IO()
main = do
       putStrLn "Example 1:"
       putStr "Result: "
       putStrLn $ show $ xor [False, True, False]
       putStrLn "Expected: True" 
       putStrLn "Example 2:"
       putStr "Result: "
       putStrLn $ show $ xor [False, True, False, False, True]
       putStrLn "Expected: False"

