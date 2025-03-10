module Calc where
-- Excersize complited untill forth chapter

import ExprT
import Parser 
import Data.Maybe

class Expr a where
      lit :: Integer -> a 
      add :: a -> a -> a
      mul :: a -> a -> a

instance Expr ExprT where
      lit n = Lit n
      add l r = Add l r
      mul l r = Mul l r

newtype MinMax = MinMax Integer deriving (Show, Eq, Ord)
newtype Mod7 = Mod7 Integer deriving (Show, Eq)

class Eval a where
      eval :: ExprT -> a

instance Eval Integer where
    eval (Lit n) = n
    eval (Mul lhs rhs) = (eval lhs) * (eval rhs)
    eval (Add lhs rhs) = (eval lhs) + (eval rhs)

instance Eval Bool where
    eval (Lit 0) = False 
    eval (Lit _) = True
    eval (Add l r) = (eval l) && (eval r)
    eval (Mul l r) = (eval l) || (eval r)

instance Eval MinMax where
    eval (Lit n) = MinMax n
    eval (Mul l r) = min (eval l) (eval r)
    eval (Add l r) = max (eval l) (eval r)

mulMod7 :: Mod7 -> Mod7 -> Mod7
mulMod7 (Mod7 a) (Mod7 b) = Mod7 $ a * b 

addMod7 :: Mod7 -> Mod7 -> Mod7
addMod7 (Mod7 a) (Mod7 b) = Mod7 $ a + b 

instance Eval Mod7 where
    eval (Lit n) = Mod7 $ mod n 7
    eval (Mul l r) = mulMod7 (eval l) (eval r)
    eval (Add l r) = addMod7 (eval l) (eval r)

evalStr :: String -> Maybe Integer
evalStr str = case partlyApplyParse of
                Nothing -> Nothing
                (Just e) -> Just $ eval e
              where partlyApplyParse = parseExp Lit Add Mul str

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

