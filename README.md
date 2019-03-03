# Assignment 3 
## Simple Haskell Math Library

#### General Information 
* This Haskell Math Library is capable of the following operations:
	1. Basic binary Addition (Add)
	2. Basic binary Multiplication (Mult)
	3. Cosine (Cos), Sine (Sin), Natural log (Ln) -- Idea to use ln: https://github.com/barskyn/CS1XA3/blob/master/Assign3/assign3/ExprType.hs
	4. Natural exponentiation (Exp)
	5. Variables (Var)
	6. Constants (Const)

* It can also perform a few basic functions:
	1. Can partially evaluate an expression
	2. Can perform partial diferrentiation (symbolically)
	3. Can perform some/partial simplification of expressions
		* Idea for how to format simplify from: https://github.com/chenc118/CS1XA3/blob/master/Assign3/ExprDiff.hs
	4. Can parse certain strings into an expression datatype 

### Additional Features 
1. Basic binary Division (Division), added to the Expr datatype directly
	2. Along with all Division instances for methods defined in ExprDiff
3. Basic binary Exponentials (Expo), added to the Expr datatype directly
	4. Along with all Expoential instances for methods defined in ExprDiff
5. Negative expression wrapper, added to the Expr datatype directly idea from: https://github.com/barskyn/CS1XA3/blob/master/Assign3/assign3/ExprType.hs
6. Implmented a function method, which attempts to apply Newtons Method for approximating roots
7. A method that attempts to find the anti-derivative of simple expressions
