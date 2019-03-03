{-|
    Module : ExprParser
    Description : Contains parsers that take a string and 
                  output an 'Expr' datatype expression 
    Copyright : (c) Akram Elwazani @2018
    License : WTFPL
    Maintainer : elwazana@mcmaster.ca
    Stability : experimental
    Portability : POSIX
    Depend : This module depends on the "ExprType" module
-}

module ExprParser (parseExprD,parseExprF) where

-- This module depends on the "ExprType" module
import ExprType

import Text.Parsec
import Text.Parsec.String

{- | parseExprD
 - --------------------------------------------
 - Parses a given string into an expression with
 - the 'Expr' datatype, of the type Expr Double
 -      Instructions :
 -          * Only takes binary constructors from the 'Expr' datataype
 -            (Add "+", Mult "*", Division "/", Expo "**")
 -          * For subtractions appropraite notation is 
 -            parseExprD "num1 + -num2" 
 -            (ie use "+" and just add a "-" infront of the number 
 -             your subtracting by)
 -}
parseExprD :: String -> Expr Double              
parseExprD ss = case parse setExprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

setExprD :: Parser (Expr Double)
setExprD = termD `chainl1` setOp

termD :: Parser (Expr Double)
termD = (negOp factorD) <|> factorD

{- | parseExprDSec
 - --------------------------------------------
 - Parses a given string into an expression with
 - the 'Expr' datatype, of the type Expr Double
 -      Instructions :
 -          * Only takes unary constructors from the 'Expr' datataype
 -            (Exp "exp", Cos "cos", Sin "sin", Ln "ln")
 -          * Operation you wish to perform must be infront of the value
 -            parseExprDSec "opValue" 
 -            * Eg. parseExprDSec "exp0"
 -            * DO NOT input nested unary expressions into parseExpreDSec
 -              * Eg. parseExprDSec "expsin1" -- Not supported
 -}
parseExprDSec :: String -> Expr Double             
parseExprDSec ss = case parse setExprDSec "" ss of 
                    Left err   -> error $ show err
                    Right expr -> expr

setExprDSec :: Parser (Expr Double)
setExprDSec = let secOp = do { op <- setOpSec; 
                               spaces;
                               term <- termDSec;
                               spaces;
                               return $ op term }
                in try secOp <|> termDSec

termDSec :: Parser (Expr Double)
termDSec = (negOp factorD) <|> factorD

factorD :: Parser (Expr Double)
factorD = try doubleParse <|> varParse

doubleParse :: Parser (Expr Double)
doubleParse = do { c <- double;
                   return $ Const c}

{- | parseExprF
 - --------------------------------------------
 - Parses a given string into an expression with
 - the 'Expr' datatype, of the type Expr Float
 -      Instructions :
 -          * Only takes binary constructors from the 'Expr' datataype
 -            (Add "+", Mult "*", Division "/", Expo "**")
 -          * For subtractions appropraite notation is 
 -            parseExprF "num1 + -num2" 
 -            (ie use "+" and just add a "-" infront of the number 
 -             your subtracting by)
 -}
parseExprF :: String -> Expr Float
parseExprF ss = case parse setExprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

setExprF :: Parser (Expr Float)
setExprF = termF `chainl1` setOp

termF :: Parser (Expr Float)
termF = (negOp factorF) <|> factorF

{- | parseExprFSec
 - --------------------------------------------
 - Parses a given string into an expression with
 - the 'Expr' datatype, of the type Expr Float
 -      Instructions :
 -          * Only takes unary constructors from the 'Expr' datataype
 -            (Exp "exp", Cos "cos", Sin "sin", Ln "ln")
 -          * Operation you wish to perform must be infront of the value
 -            parseExprFSec "opValue" 
 -            * Eg. parseExprFSec "exp0"
 -            * DO NOT input nested unary expressions into parseExpreDSec
 -              * Eg. parseExprDSec "expsin1" -- Not supported
 -}
parseExprFSec :: String -> Expr Float
parseExprFSec ss = case parse setExprFSec "" ss of 
                    Left err   -> error $ show err
                    Right expr -> expr

setExprFSec :: Parser (Expr Float)
setExprFSec = let secOp = do { op <- setOpSec; 
                               spaces;
                               term <- termFSec;
                               spaces;
                               return $ op term}
                in try secOp <|> termFSec

termFSec :: Parser (Expr Float)
termFSec = (negOp factorF) <|> factorF

factorF :: Parser (Expr Float)
factorF = try floatParse <|> varParse

floatParse :: Parser (Expr Float)
floatParse = do { c <- float;
                  return $ Const c }

-- * Micellaneous 
-- ** Functions
-- | Binary 'Expr' datatype constructors
setOp :: Parser (Expr a -> Expr a -> Expr a)
setOp = do { symbol "+"; return Add }
    <|> do { symbol "/"; return Division }
    <|> do { symbol "*"; return Mult }
    <|> do { symbol "^"; return Expo }

-- | Unary 'Expr' datatype constructors
setOpSec :: Parser (Expr a -> Expr a)
setOpSec = do { string "cos"; return Cos }
       <|> do { string "sin"; return Sin }
       <|> do { string "ln"; return Ln }
       <|> do { string "exp"; return Exp }

-- | Negative expression parser
negOp :: Parser (Expr a) -> Parser (Expr a)
negOp p = do { symbol "-";
               expr <- p;
               return $ Neg expr }

-- | Variable paraser
varParse :: Parser (Expr a)
varParse = do { var <- many1 letter;
                return $ Var var }
--------------------------------------------------------------

-- ** Uitility Combinators
symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                 dig <- digits ;
                 return (neg ++ dig) }

doubleDigits :: Parser String 
doubleDigits = do { front <- try negDigits <|> digits;
                    back  <- try decimals <|> return "";
                    return $ front ++ back }

decimals :: Parser String 
decimals = do { d  <- char '.';
                dd <- digits; 
                return $ d:dd }

double :: Parser Double
double = fmap read $ doubleDigits 

float :: Parser Float
float = fmap read $ doubleDigits
