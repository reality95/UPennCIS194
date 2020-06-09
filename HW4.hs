import Data.List

fun1 :: [Integer] -> Integer
--Finding a more native implementation of the following function:
--fun1 [] = 1
--fun1 (x:xs)
--  | even x = (x - 2) * fun1 xs
--  | otherwise = fun1 xs

fun1 xs = foldl (*) 1 (map (\x -> x - 2) (filter (even) xs))

fun2 :: Integer -> Integer
--Finding a more native implementation of the following function:
--fun2 1 = 0
--fun2 n 
--  | even n = n + fun2 (n ‘div‘ 2)
--  | otherwise = fun2 (3 * n + 1)
fun2 n = sum $ filter even (takeWhile (\x -> (x /= 1)) (iterate (\x -> if even x then (x `div` 2) else 3 * x + 1) n))

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

treeHeight :: (Tree a) -> Integer

--For purely comparative reasons the height of the Leaf will be -1
--so that we will prefer adding to the Leaf rather than to the node
--with height at least 0
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

--Adding a new value to the tree so that it remains balanced
foldTreeFunc :: a -> (Tree a) -> (Tree a)
foldTreeFunc t Leaf = Node 0 Leaf t Leaf
foldTreeFunc t (Node _ left x right)
    --We will greedily look at the children with the smallest height
    --That will guarantee that the difference between them will be
    --smaller if their height are different and will be at most
    --1 if their height are equal before adding this new element
    | hLeft < hRight    = Node (1 + (maximum [treeHeight leftCase,treeHeight right])) leftCase x right
    | otherwise         = Node (1 + (maximum [treeHeight left,treeHeight rightCase])) left x rightCase
    where 
            hLeft       = treeHeight left
            hRight      = treeHeight right
            leftCase    = foldTreeFunc t left
            rightCase   = foldTreeFunc t right

foldTree :: [a] -> (Tree a)
foldTree xs = (foldr (foldTreeFunc) Leaf xs)

xor :: [Bool] -> Bool
myNot :: Bool -> Bool -> Bool
myNot x _ = not x
--Notice that it's foldl, not foldr!!!
xor xs = foldl myNot False (filter (\x -> x) xs)

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\y ys -> [f y] ++ ys) [] xs
            
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> f y x) base (reverse xs)

findMarkedNumbers :: Integer -> [Integer]
findMarkedNumbers n =  marked
        where
             cond :: Integer -> Integer -> Integer
             cond x y = x + y + 2 * x * y --mark all i + j + 2 * i * j <= n and sort them
             marked = sort (concat (map (\i -> map (\j -> cond i j) (takeWhile (\j -> (cond i j) <= n) (iterate (\x -> x + 1) i))) [1..n]))
setDiff :: [Integer] -> [Integer] -> [Integer]
setDiff _ []            = []
setDiff [] xs           = xs
setDiff (x:xs) (y:ys)       
        | x == y        = setDiff xs ys 
        | x < y         = setDiff xs (y : ys)
        | otherwise     = y : (setDiff (x:xs) ys)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> x * 2 + 1) (setDiff (findMarkedNumbers n) [1..n])



