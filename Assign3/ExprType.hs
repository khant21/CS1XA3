{-|
Module : ExprType
Description : Contains a data type declaration for Expr a
Copyright : (c) Taha Khan @2018
License : WTFPL
Maintainer : khant21@mcmaster.ca
Stability : experimental
Portability : POSIX
-}
module ExprType where

import Data.List

{- Expression Datatype
 - -----------------------------------------
 - Wraps different operations in a
 - Expression Tree
 - Ops:
 -		Add - Standard binary addition
 -		Mult - Standard binary multiplication
 -		Div - Standard binary division **Cite AKRAM** 
 -		Const - Wrapper for simple values
 -		Cos  - Wraps the cosine function
 -		Sin  - Wraps the sine function
 -		Ln  - Wraps the natural logarithm
 -		Exp - The natural exponent 'e'
 -		Exponent - Takes a general base and exponentiates it
 -		Var - String identifier for variables
 -		Neg - A wrapper for negative 1 **Cite NOA**
-}

data Expr a = Add (Expr a) (Expr a)
			| Mult (Expr a) (Expr a)
			| Div (Expr a) (Expr a)
			| Const a
			| Cos (Expr a)
			| Sin (Expr a)
			| Ln (Expr a)
			| Exp (Expr a)
			| Exponent (Expr a) (Expr a)
			| Var String
			| Neg (Expr a)
	deriving (Eq)


{- getVars:
 -			retrieves variable identifiers from an expr			
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Const _)    = []
getVars (Var ident)  = [ident]

