{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses, InstanceSigs #-}
module Calc where

import ExprT
import Parser

newtype MyInt = MyInt Integer deriving (Eq, Show)
newtype MyBool = MyBool Bool deriving (Eq, Show)
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

class MyNum a where
    mySum, myMul :: a -> a -> a

class Expr b where
    lit :: (MyNum m) =>  m -> b
    add :: b -> b -> b
    mul :: b -> b -> b

data ExprM (MyNum m)  =  LitM      m
            | MulM      ExprM ExprM
            | AdM       ExprM ExprM

instance MyNum MyInt where
    mySum (MyInt a) (MyInt b) = MyInt (a + b)
    myMul (MyInt a) (MyInt b) = MyInt (a * b)

instance MyNum MyBool where
    mySum (MyBool a) (MyBool b) = MyBool (a || b)
    myMul (MyBool a) (MyBool b) = MyBool (a && b)

instance MyNum MinMax where
    mySum (MinMax a) (MinMax b) = MinMax (if a > b then a else b)
    myMul (MinMax a) (MinMax b) = MinMax (if a < b then a else b)
    
instance MyNum Mod7 where
    mySum (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    myMul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
    

instance Expr ExprT where
    lit  :: Integer -> ExprT
    lit x = Lit x
    add x y = Add x y
    mul x y = Mul x y

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2




evalStr :: String -> Maybe Integer
evalStr s 
    | ps == Nothing = Nothing
    | otherwise     = Just (eval js)
    where 
        ps = (parseExp Lit Add Mul s)
        Just js = ps

reify :: ExprT -> ExprT
reify = id


