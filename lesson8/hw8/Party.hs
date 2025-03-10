module Party where

import Employee
import Data.Tree

instance Semigroup GuestList where
    (<>) (GL xs funx) (GL ys funy) = (GL (xs ++ ys) (funx + funy))

instance Monoid GuestList where
    mempty = (GL [] 0)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2) = if fun1 < fun2 then gl2 else gl1

glCon :: Employee -> GuestList -> GuestList
glCon emp@(Emp _ fun) gl = (GL [emp] fun) <> gl

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v children) = f v (map (treeFold f) children)

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs em gls = moreFun boss cgls
    where boss = glCon em mempty
          cgls = foldl (<>) mempty gls

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel em gls = (glCon em mempty, fgl)
    where fgl = combineGLs em (map snd gls) 

maxFun :: Tree Employee -> GuestList
maxFun tree = moreFun (fst tf) (snd tf)
    where tf = treeFold nextLevel tree

