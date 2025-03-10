module Main where

import Party
import Employee 

printNames :: GuestList -> String
printNames gl@(GL ems tf) = foldl (\v (Emp name _) -> v ++ "\n" ++ name ) ("Total: " ++ (show tf)) ems


main :: IO()
main = do 
    str <- readFile "./company.txt"
    putStrLn $ printNames $ maxFun $ read str

