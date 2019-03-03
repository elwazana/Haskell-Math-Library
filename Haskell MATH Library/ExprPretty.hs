{-|
    Module : ExprPretty
    Description : Contains instance declaration for Show
                  for the 'Expr' datatype along with its
                  Default Methods from "ExprDiff" module
    Copyright : (c) Akram Elwazani @2018
    License : WTFPL
    Maintainer : elwazana@mcmaster.ca
    Stability : experimental
    Portability : POSIX
    Depend : This module depends on the "ExprType" module
-}

module ExprPretty where

-- This module depends on the "ExprType" module
import ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Mult e1 e2)     = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)      = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Expo e1 e2)     = parens (show e1) ++ " !^ " ++ parens (show e2)
  show (Division e1 e2) = parens (show e1) ++ " !/ " ++ parens (show e2)
  show (Sin e)          = parens $ "mySine \"" ++ (show e) ++ "\""
  show (Cos e)          = parens $ "myCosine \"" ++ (show e) ++ "\""
  show (Exp e)          = parens $ "myExp \"" ++ (show e) ++ "\""
  show (Ln e)           = parens $ "myLn \"" ++ (show e) ++ "\""
  show (Var ss)         = parens $ "var \"" ++ ss ++ "\""
  show (Const x)        = parens $ "val " ++ (show x)
