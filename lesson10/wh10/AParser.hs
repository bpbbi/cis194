module AParser where

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

data Person = Person { name :: String, age :: Int } deriving Show




----------------
per = Person { name="Artem", age= 35 }

p = Parser { runParser=(\v -> Just (v, "Ok")) }
-- runParser in this case - function getter of the fild of the structure
-- And its read like "get field runParser from structure p"
r = runParser p . name --per

