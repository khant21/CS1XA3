## Assignment 3
### README for Assignment #3 - Haskell Math Library
This assignment is an attempt to build a math library in Haskell. It is implemented using an Expr (expression) data type.

#### Base Functionality
1. The Expression datatype encodes
	- Addition
	- Multiplication
	- Division
	- Negation
	- Cos, Sin, Ln
	- Natural exponent
	- Exponentiaition
	- Variables and constants
2. Can partially evaluate an expression
3. Can perform symbolic partial differentiation
4. Can perform some simplification of expressions
5. Can parse certain strings into an expression datatype (required format is documented in the ExprParser module)

#### Additional Features I Implemented:
1. The operator for Division in the ExprType module (Implementation based on: https://github.com/elwazana/CS1XA3/tree/master/Assign3)
2. All of the additional functions implemented for the division operator (Evaluate, Simplify, Partial differentiation, etc)
3. The exponential operator in the ExprType module
4. All of the additional functions implemented for the exponential operator (Evaluate, Simplify, Partial differentiation, etc)
5. The Negation operator in the ExprType module (Implementation based on: https://github.com/barskyn/CS1XA3/tree/master/Assign3/assign3)
6. Newtons Method function which approximate the roots of an expression using an initial guess
