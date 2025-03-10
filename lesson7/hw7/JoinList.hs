module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Show, Eq)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) xs Empty = xs 
(+++) Empty ys = ys
(+++) xs ys = Append (mappend (tag xs) (tag ys)) xs ys

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m v)   = if i == (getSize $ size m) then Just v else Nothing
indexJ i (Append m l r) = case i of 
                     _  | i < gs  -> indexJ i l
                        | i == gs -> Nothing
                        | i > gs  -> indexJ i r
                         where gs = (getSize $ size m)

newtype Myint = Myint Int deriving Show
instance Semigroup Myint where
    (<>) (Myint a) (Myint b) = Myint (a + b)
instance Monoid Myint where
    mempty = Myint 1

l = Single (Myint 1) 'l'
r = Single (Myint 2) 'r'

n = l +++ r
m = l +++ JoinList.Empty

newtype MySzdInt = MySzdInt (Myint, Size) deriving Show
instance Semigroup MySzdInt where
    (<>) (MySzdInt a) (MySzdInt b) = MySzdInt ((fst a <> fst b),(snd a <> snd b))
instance Monoid MySzdInt where
    mempty = MySzdInt (Myint 0,Size 0)
instance Sized MySzdInt where
    size (MySzdInt (_, s)) = size s 

ls = Single (MySzdInt (Myint 2, Size 2)) "kek"
rs = Single (MySzdInt (Myint 6, Size 7)) "lel"
ap = Append (MySzdInt (Myint 5, Size 6)) ls rs

