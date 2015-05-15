{-# LANGUAGE FlexibleInstances #-}

module HW5.Calc
( -- exercise 1
  eval
  -- exercise 2
, evalStr
  -- exercise 3
, Expr (..)
  -- exercise 4
, MinMax (..)
, Mod7 (..)
  -- exercise 5 (optional)
, compile
  -- exercise 6 (optional)
, HasVars (var)
, VarExprT (..)
) where

import Provided.Parser (parseExp)
import Control.Applicative

import qualified Data.Map         as Map
import qualified Provided.ExprT   as ExprT
import qualified Provided.StackVM as StackVM

-- exercise 1

eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit val) = val
eval (ExprT.Mul exp1 exp2) = (eval exp1) * (eval exp2)
eval (ExprT.Add exp1 exp2) = (eval exp1) + (eval exp2)

-- exercise 2

evalStr :: String -> Maybe Integer
evalStr = parseExp id (+) (*)

-- exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT.ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- exercise 4

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>= 0)
  add = (&&)
  mul = (||)

instance Expr MinMax where
  lit = (MinMax)
  add (MinMax x) (MinMax y) = MinMax $ maximum [x, y]
  mul (MinMax x) (MinMax y) = MinMax $ minimum [x, y]

instance Expr Mod7 where
  lit = (Mod7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

-- exercise 5 (optional)

instance Expr StackVM.Program where
  lit x = [StackVM.PushI x]
  -- add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
  add = (. (++ [StackVM.Add])) . (++)
  mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul

-- exercise 6 (optional)

data (Eq a, Show a) => VarExprT a
  = Lit Integer
  | Add (VarExprT a) (VarExprT a)
  | Mul (VarExprT a) (VarExprT a)
  | Var a
  deriving (Eq, Show)

instance (Eq a, Show a) => Expr (VarExprT a) where
  lit = Lit
  add = Add
  mul = Mul

class HasVars a where
  var :: String -> a

instance HasVars (VarExprT String) where
  var = Var

instance HasVars (Map.Map String Integer -> Maybe Integer) where
  var = (Map.lookup)

-- maybeAdd :: Maybe Integer -> Maybe Integer -> Maybe Integer
-- maybeAdd x y = (<*> (+))

instance Expr (Map.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  -- add mf1 mf2 aMap = case (mf1 aMap) of
  --   Nothing -> Nothing
  --   Just x -> case (mf2 aMap) of
  --     Nothing -> Nothing
  --     Just y -> Just (x + y)
  --add mf1 mf2 m = (+) <$> mf1 m <*> mf2 m
  add mf1 mf2 m = Just (+) <*> mf1 m <*> mf2 m
  mul = mul
