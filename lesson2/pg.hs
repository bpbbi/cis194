module Pg where
--- Перечисления ---
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbege
           | King
    deriving Show

thing :: Thing
thing = King

listOfThings :: [Thing]
listOfThings = [Shoe, Ship, SealingWax]

---Типы данных ---
data FailableDouble = Failure
                    | OK Double
    deriving Show

ex1 = Failure
ex2 = OK 321.33

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = (OK $ x / y)

--- Case ---
failureToZero :: FailableDouble -> Double
failureToZero x = case x of 
                    Failure -> 0
                    OK d -> d

--- Простой паттерн матчиг --- 
data Person = Person String Int Thing

getAge :: Person -> Int
getAge (Person _ a _) = a

kate :: Person
kate = Person "Kate" 25 Shoe

artem :: Person
artem = Person "Artem" 23 King

--- Вложенные шаблоны
shoeFit :: Person -> String
shoeFit (Person _ _ Shoe) = "Shoe fit"
shoeFit (Person _ _ _)    = "Do not fit"



--- Рекурсивные типы данных ---
data Tree = Leaf Char | Node Tree Int Tree
    deriving Show

tree :: Tree
tree = Node (Node (Leaf 'q') 1 (Leaf 'w')) 3 (Leaf 'd')

reverseTree :: Tree -> Tree
reverseTree (Leaf c) = Leaf c
reverseTree (Node lhs x rhs) = (Node (reverseTree rhs) x (reverseTree lhs))

---Энум с вариабельными конструкторами, прикольно-----
data Vector = Vector1 (Int)
            | Vector2 (Int, Int)
            | Vector3 (Int, Int, Int)
            | VectorInf [Int] Int
            | VectorNone
    deriving Show

-----Паттерн мачинг, скобочки обязательны----
scale :: Vector -> Int -> Vector
scale (Vector1 (x)) scalar = Vector1 (x * scalar)
scale (Vector2 (x,y)) scalar = Vector2 (x * scalar, y * scalar)
scale (Vector3 (x,y,z)) scalar = Vector3 (x * scalar, y * scalar, z * scalar)
scale VectorNone 0 = VectorNone
scale v@(VectorNone) _ = v


vec1 :: Vector
vec1 = Vector1 23
vec2 :: Vector
vec2 = Vector2 (235, 123)
vec3 :: Vector
vec3 = Vector3 (43, 123, 23)
vecInf :: Vector
vecInf = VectorInf [312,4213,1231,1231,123213,4421,3123,123,123,43,123] 3
