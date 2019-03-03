{-|
    Module : ExprType
    Description : Contains a datatype definition defining constructors 
                  for creating common numerical expressions
    Copyright : (c) Akram Elwazani @2018
    License : WTFPL
    Maintainer : elwazana@mcmaster.ca
    Stability : experimental
    Portability : POSIX
-}

module ExprType where

import Data.List

-- | A datatype for common numeric expression 

{- | Expression DataType
 - --------------------------------------------
 - Wraps Differenet Operations in a 
 - Expression Tree
 -- | Operations:
 --     * Add      -> Standard binary addition
 --     * Mult     -> Standrard binary multiplication
 --     * Division -> Standard binary division
 --     * Expo     -> Standard binary exponentiation
 --     * Cos      -> Standard unary cosine
 --     * Sin      -> Standard unary sine
 --     * Exp      -> Standard natural exponentiation (ie to the poer of e)
 --     * Ln       -> Standard natural logarithm (ie log x with a base of e)
 --     * Neg      -> Wrapper for negative expressions
 --     * Const    -> Wrapper for simple values
 --     * Var      -> String identifier for variables
 -}
data Expr a = Add (Expr a) (Expr a)      -- ^ Binary Addition constructor
            | Mult (Expr a) (Expr a)     -- ^ Binary Multiplication contrsuctor
            | Division (Expr a) (Expr a) -- ^ Binary Division constructor
            | Expo (Expr a) (Expr a)     -- ^ Exponential constructor
            | Cos (Expr a)               -- ^ Unary Trigonometric Cosine constructor
            | Sin (Expr a)               -- ^ Unary Trigonometric Sine constructor
            | Exp (Expr a)               -- ^ Unary Natural Exponential constructor
            | Ln (Expr a)                -- ^ Unary Natural Logarithmic constructor
            | Neg (Expr a)               -- ^ Unary Negative Expression Wrapper
            | Const a                    -- ^ Value Wrapper
            | Var String                 -- ^ Variable Identifier
  deriving Eq

-- | Micellaneous Function

{- | getVars : 
 -      retrieves variable identifiers form
 -      an Expr 
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)      = getVars e1 `union` getVars e2
getVars (Mult e1 e2)     = getVars e1 `union` getVars e2
getVars (Division e1 e2) = getVars e1 `union` getVars e2
getVars (Cos e)          = getVars e 
getVars (Sin e)          = getVars e 
getVars (Expo e1 e2)     = getVars e1 `union` getVars e2 
getVars (Exp e)          = getVars e
getVars (Ln e)           = getVars e
getVars (Neg e)          = getVars e
getVars (Const _)        = []
getVars (Var ident)      = [ident]