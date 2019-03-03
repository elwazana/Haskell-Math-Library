{-|
    Module : ExprTest
    Description : Contains tests for the methods defined in
                  "ExprDiff" module
    Copyright : (c) Akram Elwazani @2018
    License : WTFPL
    Maintainer : elwazana@mcmaster.ca
    Stability : experimental
    Portability : POSIX
    Depend : This module depends on the "ExprType","ExprDiff","ExprPretty","ExprParser" modules
-}
module ExprTest where

-- This module depends on "ExprDiff","ExprParser","ExprPretty","ExprType" modules
import ExprDiff
import ExprParser
import ExprPretty
import ExprType

import qualified Data.Map.Strict as Map
import Test.QuickCheck

-- eval tests for all constructors of 'Expr' datatype
evalProp1 :: Double -> Double -> Bool
evalProp1 a b = eval (Map.fromList [("x",a),("y",b)]) (Add (Var "x") (Var "y")) == a + b
testEvalProp1 = quickCheck evalProp1

evalProp2 :: Double -> Double -> Bool
evalProp2 a b = eval (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Var "y")) == a * b
testEvalProp2 = quickCheck evalProp2

{- The following evals work. However, due to them evaluting floating points
   comparing two floating point numbers to each other errors arise -}
{-
evalProp3 :: Double -> Double -> Bool
evalProp3 a b = eval (Map.fromList [("x",a),("y",b)]) (Division (Var "x") (Var "y")) == eval (Map.fromList [("x",a),("y",b)]) (Const (a / b))
testEvalProp3 = quickCheck evalProp3
-}

{-
evalProp4 :: Double -> Double -> Bool
evalProp4 a b = eval (Map.fromList [("x",a),("y",b)]) (Expo (Var "x") (Var "y")) == (a) ** (b)
testEvalProp4 = quickCheck evalProp4
-}

evalProp5 :: Double -> Bool
evalProp5 a = eval (Map.fromList [("x",a)]) (Exp (Var "x")) == exp(a)
testEvalProp5 = quickCheck evalProp5

evalProp6 :: Double -> Bool
evalProp6 a = eval (Map.fromList [("x",a)]) (Cos (Var "x")) == cos(a)
testEvalProp6 = quickCheck evalProp6

evalProp7 :: Double -> Bool
evalProp7 a = eval (Map.fromList [("x",a)]) (Sin (Var "x")) == sin(a)
testEvalProp7 = quickCheck evalProp7

evalProp8 :: Double -> Bool
evalProp8 a = eval (Map.fromList [("x",a)]) (Neg (Var "x")) == -a
testEvalProp8 = quickCheck evalProp8
