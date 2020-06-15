module LinkedList where

import Sized

data JoinList m a = Empty
        | Single m a
        | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ jl = jl
(Single x y) +++ jl = Append (mappend x (tag jl)) (Single x y) jl
(Append _ left right) +++ jl = Append (mappend (tag left) (tag newRight)) left newRight
        where newRight = right +++ jl

--Safe indexing
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ y) = Just y
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ left right)
        | i < leftSize      = indexJ i left
        | otherwise         = indexJ (i - leftSize) right
        where leftSize = getSize $ size $ tag left

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 (Single x y) = Single x y
dropJ 1 (Single _ _) = Empty
dropJ n (Append _ left right)
        | n < leftSize          = Append (mappend (tag newLeft) (tag right)) newLeft right
        | otherwise             = dropJ (n - leftSize) right
        where 
                leftSize    = getSize $ size $ tag left
                newLeft     = dropJ n left

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ _ (Single x y) = Single x y
takeJ n (Append _ left right)
        | n >= leftSize         = Append (mappend (tag left) (tag newRight)) left newRight
        | otherwise             = takeJ n left
        where
                leftSize    = getSize $ size $ tag left
                newRight    = takeJ n right
