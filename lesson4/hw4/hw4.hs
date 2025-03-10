module Main where

{-
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
 a| even x = (x - 2) * fun1 xs
| otherwise = fun1 xs
-}

fun1 :: [Integer] -> Integer
fun1 = foldl (*) 1 . map (flip (-) 2) . filter even

{-
fun1 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n ‘div‘ 2)
       | otherwise = fun2 (3 * n + 1)
-}

fun2 :: Integer -> Integer
fun2 = foldl (+) 0 . filter even . takeWhile ( /= 1) . iterate (\x -> if even x then div x 2 else x * 3 + 1)
{-
 - Exercise 2:
 -}
data Tree a = Leaf
            | Node Int (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree xs = foldr (growTree) Leaf xs

growTree :: a -> Tree a -> Tree a
growTree value Leaf = Node 0 Leaf value Leaf
growTree value (Node n lhs prevval rhs)
                                      | graterThen lhs rhs = Node (n+1) lhs prevval (growTree value rhs)
                                      | otherwise = Node (n+1) (growTree value lhs) prevval rhs

graterThen :: Tree a -> Tree a -> Bool
graterThen Leaf _ = False
graterThen _ Leaf = True
graterThen (Node ln _ _ _) (Node rn _ _ _) = ln >= rn

getDepth :: Int -> Int
getDepth 1 = 1
getDepth n = length $ takeWhile (>1) $ iterate (\x -> div (x+1) 2) n

setIndexTree :: Tree a -> Tree a
setIndexTree Leaf = Leaf
setIndexTree (Node n lhs v rhs) = (Node (getDepth n) (setIndexTree lhs) v (setIndexTree rhs))

printTree :: Show a => Tree a -> String -> String
printTree Leaf pad = pad ++ "Leaf\n"
printTree (Node a lhs val rhs) pad = 
	   (printTree rhs $ pad ++ "    ") 
	++ pad ++ "N= " ++ show a ++ "\n"
        ++ pad ++ "V= " ++ show val ++ "\n" 
	++ (printTree lhs $ pad ++ "    ")


main :: IO()
main = do 
       let v = ['a','b','c','d','e','f','g','h','j']
       putStrLn $ flip printTree "" $ setIndexTree $ foldTree v 
