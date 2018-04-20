{-|
Module : ExprPretty
Description : Contains instance declarations for show for each Expr a type
Copyright : (c) Taha Khan @2018
License : WTFPL
Maintainer : khant21@mcmaster.ca
Stability : experimental
Portability : POSIX
This module depends on the "ExprType" Module.
-}
module ExprPretty where
import ExprType
{- Instance Show Expr
 -		Provides a pretty representation of our datatype
 - 		Matching the DSL provided in DiffExpr
 -}
parens :: String -> String
parens ss = "(" ++ ss ++ ")"

instance Show a => Show (Expr a) where
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Div e1 e2)  = parens (show e1) ++ " !/ " ++ parens (show e2)
  show (Exponent e1 e2)  = parens (show e1) ++ " !^ " ++ parens (show e2)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x
  show (Exp x)    = " expo " ++ parens (show x)
  show (Sin x)    = " sin1 " ++ parens (show x)
  show (Cos x)    = " cos1 " ++ parens (show x)
  show (Ln x)    = " ln1 " ++ parens (show x)