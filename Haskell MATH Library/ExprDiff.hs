{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
    Module : ExprDiff
    Description : Contains instance declarations
                  along with type class definition
                  for differentible expressions
    Copyright : (c) Akram Elwazani @2018
    License : WTFPL
    Maintainer : elwazana@mcmaster.ca
    Stability : experimental
    Portability : POSIX
    Depend : This module depends on the "ExprType" module
-}

module ExprDiff where

-- This module depends on the "ExprType" module
import ExprType

import qualified Data.Map.Strict as Map


{- | Class DiffExpr:
 -      Differentiable Expressions
 - ----------------------------------------
 - This class has methods over the 'Expr' datatype
 - that aid with construction and evaluation of
 - differentiable expressions
 - -----------------------------------------
 - Methods:
 - 'eval' : Takes a dictionary of variable identifiers
          and values, and uses it to compute the Expr
          fully
 - 'simplify' : Takes a possibly incomplete dictionary and
              uses it to reduce Expr as much as possible
              Add (Add (Var "x") (Const 1)) (Add (Const 2) (Var y))
              => Add (Constant) (Add (Var "x") (Var "y")) <-- IDEALLY 
              (Practical application this may not be the case)
 - 'partDiff' : Given a var identifier, differentiate IN TERMS of
              that identifier
 - 'partAntiDerivative' : Given a var identifier, attempts to find the a
                        anti-derivative IN TERMS of that identifier
                        Note : Only works on simple expressions with 
                               simple anti-derivatives
 - 'newtonsMethod' : Takes a dictionary with a single variable identifer 
                   and its value, and uses that value as the intial guess
                   in an attempt to execute newtons method for finding 
                   possible roots
                   Note : Beware of using an expression with a partDiff of
		          Const 0
 - Default Methods
 -      !+, !*, var, val, myCos, mySin, myExp, myLn : are function wrappers for 
 -      Expr constructors that performs additional simplifications
 -}
class DiffExpr a where
    -- | Evaluates and expression using a dictionary containing variable identifiers and those variables values
    eval :: Map.Map String a -> Expr a -> a

    -- | Attempts to simplify the expression
    simplify :: Map.Map String a -> Expr a -> Expr a

    -- | Attempts to partially differentiate an expression in terms of a single variable
    partDiff :: String -> Expr a -> Expr a

    -- | Attempts to partially integrate an expression in terms of a single variable
    partAntiDerivative :: String -> Expr a -> Expr a

    -- | Attempts to execute newtons method for finding roots with the intial guess being the value of the variable in a dictionary
    newtonsMethod :: Map.Map String a -> String -> Expr a -> a

    -- | Default Methods that come with the 'Expr' datatype and "ExprDiff"
    (!+) :: Expr a -> Expr a -> Expr a
    e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2

    (!*) :: Expr a -> Expr a -> Expr a
    e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2

    (!^) :: Expr a -> Expr a -> Expr a
    e1 !^ e2 = simplify (Map.fromList []) $ Expo e1 e2

    (!/) :: Expr a -> Expr a -> Expr a
    e1 !/ e2 = simplify (Map.fromList []) $ Division e1 e2

    val :: a -> Expr a
    val x = Const x

    var :: String -> Expr a
    var x = Var x

    myCosine :: Expr a -> Expr a
    myCosine x = Cos x
    
    mySine :: Expr a -> Expr a
    mySine x = Sin x
    
    myExp :: Expr a -> Expr a
    myExp x = Exp x
    
    myLn :: Expr a -> Expr a
    myLn x = Ln x


{- | Most intuitive instances for DiffExpr:
 -      Instances of Differentiable Expressions
 - ----------------------------------------
 - Methods:
 - 'eval' : Evaluates an expression of 'Expr' datatype, using a dictionary to find the values
            of any variables in the expression
            Eg. eval (Map.fromList [list of tuples (ie dictionary)]) (expression)
                Note: the dictionary must posses values for all variables in the expression
                      for the expression to be evaluated fully
 - 'simplify' : Attempts to simplify an expression of 'Expr' datatype
                Eg. simplify (Map.fromList [list of tuples (ie dictionary)]) (expression)
 - 'partDiff' : Attempts to partially diferrentiate and expression of 
                'Expr' datatype, in terms of a given variable
                Eg. partDiff "variableToDiff" (expression) 
 - 'partAntiDerivative' : Attempts to partially integrate (same as partDiff but reversed) 
                          an expression of 'Expr' datatype
                          Eg. partAntiDerivative "variableToIntegrate" (expression)
                            Note: ONly works for simple expressions with commonly known integrals
 - 'newtonsMethod' : Attempts to implement newtons method (x2 = x1 - f(x1)/f'(x1)) for an
                     expression of 'Expr' datatype, using partDiff
                     Eg. newtonsMethod (Map.fromList [list of tuples (ie dictionary)]) "variableToDiff" (expression)
 -}
instance (Floating a,Eq a) => DiffExpr a where
    -- ^ Evaultes 'Add' of type 'Expr'
    eval vrs (Add e1 e2)      = eval vrs e1 + eval vrs e2      
    
    -- ^ Evaultes 'Mult' of type 'Expr'
    eval vrs (Mult e1 e2)     = eval vrs e1 * eval vrs e2      

    -- ^ Evaultes 'Division' of type 'Expr'
    eval vrs (Division e1 e2) = (eval vrs e1) / (eval vrs e2)  

    -- ^ Evaultes 'Cos' of type 'Expr'
    eval vrs (Cos e)          = cos (eval vrs e)   

    -- ^ Evaultes 'Sin' of type 'Expr'
    eval vrs (Sin e)          = sin (eval vrs e)        

    -- ^ Evaultes 'Expo' of type 'Expr'
    eval vrs (Expo e1 e2)     = (eval vrs e1) ** (eval vrs e2) 

    -- ^ Evaultes 'Exp' of type 'Expr'
    eval vrs (Exp e)          = exp (eval vrs e) 

    -- ^ Evaultes 'Ln' of type 'Expr'    
    eval vrs (Ln e)           = log (eval vrs e) 

    -- ^ Evaultes 'Neg' of type 'Expr'
    eval vrs (Neg e)          = (-1) * (eval vrs e)      

    -- ^ Evaultes 'Const' of type 'Expr'
    eval vrs (Const x)        = x       

    -- ^ Evaultes 'Var' of type 'Expr' 
    eval vrs (Var x)          = case Map.lookup x vrs of  
                                        Just v  -> v
                                        Nothing -> error "failed lookup in eval"


    {- | Pattern matches attempting partially differentiation for 'partDIff' 
         on all constructors of 'Expr' datatype
    -}
    partDiff str (Add e1 e2)      = Add (partDiff str e1) (partDiff str e2)
    partDiff str (Mult e1 e2)     = Add (Mult (partDiff str e1) e2) (Mult e1 (partDiff str e2))
    partDiff str (Division e1 e2) = Division (Add (Mult (partDiff str e1) e2) (Mult (Neg e1) (partDiff str e2))) (Expo (e2) (Const 2))
    partDiff str (Cos e)          = Mult (Neg (Sin e)) (partDiff str e)
    partDiff str (Sin e)          = Mult (Cos e) (partDiff str e)
    partDiff str (Expo e1 e2)     = Mult (Ln e1) (Mult (Expo e1 e2) (partDiff str e2)) -- Only for bases as numbers not variables
    partDiff str (Exp e)          = Mult (Exp e) (partDiff str e)
    partDiff str (Ln e)           = Division (partDiff str e) (e)
    partDiff str (Neg e)          = Neg (partDiff str e)
    partDiff str (Const x)        = Const 0
    partDiff str (Var x)          | x == str  = (Const 1)
                                  | otherwise = (Const 0)


    -- | Pattern matches for 'simplify' on all constructors of 'Expr' datatype 
    simplify vrs (Const e) = Const e
    simplify vrs (Neg e)   = Const (eval vrs (Neg e))
    simplify vrs (Var x)   = case Map.lookup x vrs of 
                                Just x' -> Const (eval vrs (Var x))
                                Nothing -> Var x
    simplify vrs (Add e1 e2)               = let 
                                                    e1' = simplify vrs e1
                                                    e2' = simplify vrs e2
                                                in case (e1',e2') of 
                                                    (Const a,Const b) -> Const (eval vrs (Add e1' e2'))
                                                    (Const 0,e2')     -> e2' 
                                                    (e1',Const 0)     -> e1'
                                                    (e1',e2')         -> Add e1' e2'
    simplify vrs (Mult e1 e2)               = let 
                                                    e1' = simplify vrs e1
                                                    e2' = simplify vrs e2
                                                in case (e1',e2') of 
                                                    (Const a,Const b) -> Const (eval vrs (Mult e1' e2'))
                                                    (Const 0,e2')     -> Const (0.0) 
                                                    (e1',Const 0)     -> Const (0.0)
                                                    (e1',e2')         -> Mult e1' e2'
    simplify vrs (Division e1 e2)           = let 
                                                    e1' = simplify vrs e1
                                                    e2' = simplify vrs e2
                                                in case (e1',e2') of 
                                                    (Const 0,Const 0)  -> Const (eval vrs (Division e1' e2'))
                                                    (Const a,Const b)  -> Const (eval vrs (Division e1' e2'))
                                                    (Const 0,e2')      -> Const (0.0) 
                                                    (e1',Const 0)      -> Var "Infinity"
                                                    (e1',e2')          -> Division e1' e2'
    simplify vrs (Expo e1 e2)               = let 
                                                    e1' = simplify vrs e1
                                                    e2' = simplify vrs e2
                                                in case (e1',e2') of 
                                                    (Const a,Const b)  -> Const (eval vrs (Expo e1' e2'))
                                                    (Const 0,e2')      -> Const (0.0) 
                                                    (e1',Const 0)      -> Const (1.0) 
                                                    (e1',e2')          -> Expo e1' e2'
    simplify vrs (Exp e)                    = let 
                                                    e' = simplify vrs e
                                                in case e' of 
                                                    Const a -> Const (eval vrs (Exp e'))
                                                    e'      -> Exp e'
    simplify vrs (Sin e)                    = let 
                                                    e' = simplify vrs e
                                                in case e' of 
                                                    Const a -> Const (eval vrs (Sin e'))
                                                    e'      -> Sin e'
    simplify vrs (Cos e)                    = let 
                                                    e' = simplify vrs e
                                                in case e' of 
                                                    Const a -> Const (eval vrs (Cos e'))
                                                    e'      -> Cos e'


    {- | Pattern matches attempting partially integrations for 'partAntiDerivative' 
         on all constructors of 'Expr' datatype
    -}                      
    partAntiDerivative str (Const e)                     = Mult (Const e) (Var str)
    partAntiDerivative str (Expo (Const e1) (Const e2))  = Mult (Expo (Const e1) (Const e2)) (Var str)
    partAntiDerivative str (Division (Const a) (Var x))  | x == str  = Mult (Const a) (Ln (Var str))
                                                         | otherwise = Division (Const a) (Var x) 
    partAntiDerivative str (Expo (Var x) (Const n))      = Division (Expo (Var x) (Const (n+1))) (Const (n+1))
    partAntiDerivative str (Add e1 e2)                   = Add (partAntiDerivative str e1) (partAntiDerivative str e2) 
    partAntiDerivative str (Exp (Var x))                 | x == str  = Exp (Var str)
                                                         | otherwise = Exp (Var x)
    partAntiDerivative str (Exp (Const e))               = Mult (Exp (Const e)) (Var str)
    partAntiDerivative str (Sin (Var x))                 | x == str  = Neg (Cos (Var str))
                                                         | otherwise = Sin (Var x)
    partAntiDerivative str (Sin (Const e))               = Mult (Sin (Const e)) (Var str)
    partAntiDerivative str (Cos (Var x))                 | x == str  = Sin (Var str)
                                                         | otherwise = Cos (Var x)
    partAntiDerivative str (Cos (Const e))               = Mult (Cos (Const e)) (Var str)
    partAntiDerivative str (Mult (Const a) e2)           = Mult (Const a) (partAntiDerivative str e2) 
    partAntiDerivative str (Var "x")                     = Division (Expo (Var "x") (Const 2)) (Const 2)

    -- | Newtons method implementation 
    newtonsMethod vrs str expr = case Map.lookup str vrs of
                                    Just v -> v - ((eval vrs expr) / (eval vrs (partDiff str expr)))
                                    Nothing -> error "Falied to identify intial guess"
