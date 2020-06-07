myAbs :: Int -> Int
myAbs x
    |x < 0 = -x
    |otherwise = x
myAbss :: [Int] -> [Int]

myAbss xs = map f xs

myQuickSort :: [Int] -> [Int]

myQuickSort [] = []

myQuickSort xs = myQuickSort xl ++ xm ++ myQuickSort xr
    where   n = length xs
            middle = xs !! ((n - 1) `div` 2)
            xl = filter (\x -> x < middle) xs
            xm = filter (\x -> x == middle) xs
            xr = filter (\x -> x > middle) xs

merge :: ([Int],[Int]) -> [Int]

--base cases
merge ([],[]) = []
merge ([],xs) = xs
merge (xs,[]) = xs

merge (xl,xr)
    |l < r = [l] ++ merge(drop 1 xl,xr)
    |otherwise = [r] ++ merge(xl, drop 1 xr)
    where l = xl !! 0
          r = xr !! 0

myMergeSort :: [Int] -> [Int]

myMergeSort [] = []
myMergeSort [x] = [x]

myMergeSort xs = merge(myMergeSort(xl),myMergeSort(xr))
    where n = length xs
          xl = take (n `div` 2) xs
          xr = drop (n `div` 2) xs

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x < 1      = []
    | otherwise  = [x `mod` 10] ++ toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer] 
toDigits x
    | x < 1     = []
    | otherwise  = toDigits (x `div` 10) ++ [x `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : (y * 2) : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ (map (sum . toDigits) xs)

validate :: Integer -> Bool
validate xs = (sumDigits $ doubleEveryOther $ toDigitsRev xs) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
        | n <= 0    = []
        | otherwise = (hanoi (n - 1) a c b) ++ [(a,b)] ++ (hanoi (n - 1) c b a)

--main = interact $ unlines . map show . fs . map read . words

