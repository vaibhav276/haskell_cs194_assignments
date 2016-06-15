{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser
import qualified StackVM

eval :: ExprT -> Integer
eval (Lit l) = l
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)

evalStr :: String -> Maybe Integer
evalStr s = case (parseExp Lit Add Mul s) of
   Nothing  -> Nothing
   Just e   -> Just (eval e)

class Expr a where
   lit :: Integer -> a
   add :: a -> a -> a
   mul :: a -> a -> a

instance Expr ExprT where
   lit l = Lit l
   add a b = Add a b
   mul a b = Mul a b

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
   lit l = l
   add a b = a + b
   mul a b = a * b

instance Expr Bool where
   lit l = (l >= 0)
   add a b = a || b
   mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
   lit l = MinMax l
   add (MinMax a) (MinMax b) = MinMax (max a b)
   mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
   lit l = Mod7 (mod l 7)
   add (Mod7 a) (Mod7 b) = Mod7 (mod (a+b) 7)
   mul (Mod7 a) (Mod7 b) = Mod7 (mod (a*b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr StackVM.Program where
   lit l = [StackVM.PushI l]
   add a b = a ++ b ++ [StackVM.Add]
   mul a b = a ++ b ++ [StackVM.Mul]
