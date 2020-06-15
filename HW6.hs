{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
import Data.Ratio
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x nxt) = x : (streamToList nxt)

instance Show a => Show (Stream a) where
    show b = show $ take 20 $ streamToList $ b

tailStream :: Stream a -> Stream a
tailStream (Stream x nxt) = nxt

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x nxt) = Stream (f x) (streamMap f nxt)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibLinear :: (Integer,Integer) -> (Integer,Integer)
fibLinear (x,y) = (y,x + y)

genFib :: (Integer,Integer) -> [Integer]
genFib (x,y) = x : (genFib $ fibLinear $ (x,y))

fibs2 :: [Integer]
fibs2 = genFib (0,1)

nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) 0

largestPowerOfTwo :: Integer -> Integer
largestPowerOfTwo 0 = undefined
largestPowerOfTwo n
    | n `mod` 2 == 0    = 1 + largestPowerOfTwo (n `div` 2)
    | otherwise         = 0

ruler :: Stream Integer
--Do not forget the tail to exclude number 0
ruler = streamMap largestPowerOfTwo (tailStream nats)

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Stream n (streamRepeat 0)
    negate (Stream n nxt) = Stream (-n) (negate nxt)
    (Stream a nxt1) + (Stream b nxt2) = Stream (a + b) (nxt1 + nxt2)
    (Stream a nxt1) - (Stream b nxt2) = Stream (a - b) (nxt1 - nxt2) 
    (Stream a nxt1) * (Stream b nxt2) = Stream (a * b) ((fromInteger a) * nxt2 + (nxt1 * (Stream b nxt2)))

getNStream :: Stream a -> Int -> [a]
getNStream s n = take n (streamToList s)

instance Fractional (Stream Integer) where
    (Stream 0 nxt1) / (Stream 0 nxt2) = nxt1 / nxt2
    (Stream _ _) / (Stream 0 _) = undefined
    (Stream a nxt1) / (Stream b nxt2) = q
            where q = Stream (a `div` b) ((nxt1 - q * nxt2) / (fromInteger b))
    fromRational x = (fromInteger $ numerator x) / (fromInteger $ denominator x)
    recip s = (fromInteger 1) / s

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

--The elements are a11,a12,a21,a22 in this order
data Mat22 = Mat22 Integer Integer Integer Integer
    deriving (Show, Eq)
instance Num Mat22 where
    negate (Mat22 a11 a12 a21 a22) = (Mat22 (-a11) (-a12) (-a21) (-a22))
    fromInteger n = (Mat22 n 0 n 0)
    (Mat22 a11 a12 a21 a22) + (Mat22 b11 b12 b21 b22) = (Mat22 (a11 + b11) (a12 + b12) (a21 + b21) (a22 + b22))
    (Mat22 a11 a12 a21 a22) * (Mat22 b11 b12 b21 b22) = (Mat22 c11 c12 c21 c22)
                    where
                            c11 = a11 * b11 + a12 * b21
                            c12 = a12 * b22 + a11 * b12
                            c21 = a21 * b11 + a22 * b21
                            c22 = a22 * b22 + a21 * b12
fib4 :: Integer -> Integer
fib4 n = c12
        where (Mat22 _ c12 _ _) = (Mat22 1 1 1 0) ^ n 
