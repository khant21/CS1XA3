{-|
Module : ExprTest
Description : Contains test for the functions in 'ExprDiff'
Copyright : (c) Taha Khan @2018
License : WTFPL
Maintainer : khant21@mcmaster.ca
Stability : experimental
Portability : POSIX
This module depends on the ALL Modules.
-}
module ExprTest where

import           ExprDiff
import           ExprParser
import           ExprPretty
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck


-- * Test cases for the Eval function

-- ^ eval test for the Add expression type
evalProp1 :: Double -> Double -> Bool
evalProp1 a b = eval (Map.fromList [("x",a),("y",b)]) (Add (Var "x") (Var "y")) == a+b
testevalProp1 = quickCheck evalProp1

-- ^ eval test for the Mult expression type
evalProp2 :: Double -> Double -> Bool
evalProp2 a b = eval (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Var "y")) == a*b
testevalProp2 = quickCheck evalProp2

-- ^ eval test for the natural exponent expression type
evalProp3 :: Double -> Bool
evalProp3 a= eval (Map.fromList [("x",a)]) (Exp (Var "x")) == exp(a)
testevalProp3 = quickCheck evalProp3

-- ^ eval test for the Exponent expression type
evalProp4 :: Double -> Double -> Bool
evalProp4 a b = eval (Map.fromList [("x",a),("y",b)]) (Exponent (Var "x") (Var "y")) == a ** b
testevalProp4 = quickCheck evalProp4

-- ^ eval test for the Var expression type
evalProp5 :: Double -> Double -> Bool
evalProp5 a b = eval (Map.fromList [("x",a),("y",b)]) (Var "y") == b
testevalProp5 = quickCheck evalProp5

-- ^ eval test for the Neg expression type
evalProp6 :: Double -> Double -> Bool
evalProp6 a b = eval (Map.fromList [("x",a),("y",b)]) (Neg (Const a)) == -a
testevalProp6 = quickCheck evalProp6

-- ^ eval test for the Cos expression type
evalProp7 :: Double -> Double -> Bool
evalProp7 a b = eval (Map.fromList [("x",a),("y",b)]) (Cos (Var "x")) == cos (a)
testevalProp7 = quickCheck evalProp7

-- ^ eval test for the Sin expression type
evalProp8 :: Double -> Double -> Bool
evalProp8 a b = eval (Map.fromList [("x",a),("y",b)]) (Sin (Var "x")) == sin (a)
testevalProp8 = quickCheck evalProp8