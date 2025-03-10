module Hw3 where
--Skips
skips :: [a] -> [[a]]
skips [] = []
skips xs = map keepModNthElem 
            $ zip [1..] $ xs >> [xs] 

keepModNthElem :: (Int, [a]) -> [a]
keepModNthElem (n, xs) = map snd 
                            $ filter (\(xn, _) -> mod xn n == 0) 
                                $ zip [1..] xs

-- localMaxima
localMaxima :: [Integer] -> [Integer]
localMaxima xs 
            | length xs < 3 = []
            | otherwise = map (\(_,y,_) -> y) 
                        $ filter (\(x,y,z) -> y > x &&  y > z) 
                        $ zip3 xs (tail xs) (tail $ tail xs) 

-- Hystogram
testHyst = [2,3,7,1,3,6,3,4,7,8,9,0,9,5,6,7,8,1,2,4,6,8,4,2,1,3,5]
axysx = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

histogram :: [Integer] -> String
histogram [] = "Not enoth data"
histogram xs = "" 
