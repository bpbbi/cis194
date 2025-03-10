module Functors where
-- Функтор описывает как применять функции к структуре
data Jl m a = Empty
            | Single m a
            | Append m (Jl m a) (Jl m a)
    deriving (Show)

a = Single 10 10

data Funny f a = Funny a (f a)

foo :: Funny Maybe Int -> Maybe Int
foo (Funny _ a) = Just (Just a)

