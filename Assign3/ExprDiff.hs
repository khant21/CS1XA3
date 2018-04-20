{-|
Module : ExprDiff
Description : Contains a type class and instances for
differentiable expressions
Copyright : (c) Taha Khan @2018
License : WTFPL
Maintainer : khant21@mcmaster.ca
Stability : experimental
Portability : POSIX
This module depends on the "ExprType" Module. It uses the 'Expr' datatype and
performs various functions over the differential expressions them. 
-- | The base functions include:
--      * 'eval'
--      * 'simplify'
--      * 'partDiff'
-- | Additional functions include:
--      * 'newton'
-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff where

import qualified Data.Map as Map
import ExprType


-- * Type Class Declaration

{- | Class DiffExpr:
 -      Differentiable Expressions
 - ----------------------------------------
 - This class has methods over the Expr datatype
 - that assist with construction and evaluation of
 - differentiable expressions
 - -----------------------------------------
 - Methods:
 - 'eval' : takes a dictionary of variable identifiers
            and values, and uses it to compute the Expr
            fully
            EX. eval (Map.fromList [("x", 0)]) (Mult (Const 2) (Mult (Const 2) (Const 2)))
            => 8.0
 - 'simplify' : Takes a possibly incomplete dictionary and
                uses it to reduce Expr as much as possible
                EX. Add (Add (Var "x") (Const 1)) (Add (Const 2) (Var y))
                => Add (Constant) (Add (Var "x") (Var "y")) <-- IDEALLY (Practically, this may not be the case)
 - 'partDiff' : Given a var identifier, differentiate IN TERMS of
                that identifier
                EX. partDiff "x" (Mult (Neg (Const 2)) (Var "x"))
                => Add (Mult (Mult (Neg (Const 1.0)) (Const 0.0)) (Var "x")) (Mult (Neg (Const 2.0)) (Const 1.0))
 - 'newton' : Takes a dictionary of variable identifiers and values, a string which represents
              what variable to differentiate in terms of, an Expr a, and an a which respresents
              the intial guess for the x value of the root.
              EX. newton (Map.fromList [("x", 1)]) "x" (Add (Var "x") (Neg (Const 5))) 1
              => 5.0
              => Note: 'newton' only performs one iteration of newtons method to approximate the root
 - Default Methods
 -      '!+', '!*', 'var', 'val' : are function wrappers for Expr constructors that
 -      performs additional simplifications
 -}
class DiffExpr a where
  
  -- | Evaluate an expression given a dictionary of variables values
  eval :: Map.Map String a -> Expr a -> a

  -- | Simplify an expression by substituting in the variable values
  simplify :: Map.Map String a -> Expr a -> Expr a
  
  -- | Partially differentiate a fucntion with respect to a certain identifier
  partDiff :: String -> Expr a -> Expr a
  
  -- | Approximate the roots of an expression using Newtons Method
  newton :: Map.Map String a -> String -> Expr a -> a -> a

  {- |  The Following are are default methods for all 'Expr' types
   - 
   -}
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2
  
  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2
  
  (!/) :: Expr a -> Expr a -> Expr a
  e1 !/ e2 = simplify (Map.fromList []) $ Div e1 e2

  (!^) :: Expr a -> Expr a -> Expr a
  a !^ b = Exponent a b

  expo :: Expr a -> Expr a
  expo a = Exp a

  val :: a -> Expr a
  val x = Const x
  
  var :: String -> Expr a
  var x = Var x

  cos1 :: Expr a -> Expr a
  cos1 a = Cos a

  sin1 :: Expr a -> Expr a
  sin1 a = Sin a

  ln1 :: Expr a -> Expr a
  ln1 a = Ln a



-- * Instances of the 'DiffExpr' type class
{- Most intuitive instance of 'DiffExpr'
 - Methods
 -      'eval' : Attempts to evaluate an expression based on given values for variables
 -      'simplify' : Attempts to simplify an 'Expr' datatype for all constructors
 -      'partDiff' : Partially differentiates an 'Expr' datatype.
 -      'newton' : Pattern matches over all Expr types. Uses the formula x1 = x0 - (f(x0)/f'(x0))
 -                 Where x0 is the initial guess an f(x0) and f'(x0) uses eval and partDiff,
 -                 respectively
 -}
instance (Floating a, (Eq a)) => DiffExpr a where
  
  -- ^ Performs Newtons Method for a 'Mult' 'Expr' type
  newton vrs str (Mult e1 e2) initialGuess = initialGuess - ((eval vrs (Mult e1 e2)) / (eval vrs (partDiff str (Mult e1 e2))))
  
  -- ^ Performs Newtons Method for a 'Add' 'Expr' type
  newton vrs str (Add e1 e2) initialGuess = initialGuess - ((eval vrs (Add e1 e2)) / (eval vrs (partDiff str (Add e1 e2))))
  
  -- ^ Performs Newtons Method for a 'Div' 'Expr' type
  newton vrs str (Div e1 e2) initialGuess = initialGuess - ((eval vrs (Div e1 e2)) / (eval vrs (partDiff str (Div e1 e2))))
  
  -- ^ Performs Newtons Method for an 'Exponent' 'Expr' type
  newton vrs str (Exponent e1 e2) initialGuess = initialGuess - ((eval vrs (Exponent e1 e2)) / (eval vrs (partDiff str (Exponent e1 e2))))
  
  -- ^ Performs Newtons Method for a 'Neg' 'Expr' type
  newton vrs str (Neg e1) initialGuess = initialGuess - ((eval vrs (Neg e1)) / (eval vrs (partDiff str (Neg e1))))
  
  -- ^ Performs Newtons Method for a 'Const' 'Expr' type
  newton vrs str (Const a) initialGuess = a
  
  -- ^ Performs Newtons Method for a 'Sin' 'Expr' type
  newton vrs str (Sin e1) initialGuess = initialGuess - ((eval vrs (Sin e1)) / (eval vrs (partDiff str (Sin e1))))
  
  -- ^ Performs Newtons Method for a 'Cos' 'Expr' type
  newton vrs str (Cos e1) initialGuess = initialGuess - ((eval vrs (Cos e1)) / (eval vrs (partDiff str (Cos e1))))
  
  -- ^ Performs Newtons Method for a 'Ln' 'Expr' type
  newton vrs str (Ln e1) initialGuess = initialGuess - ((eval vrs (Ln e1)) / (eval vrs (partDiff str (Ln e1))))
  
  -- ^ Performs Newtons Method for a 'Exp' 'Expr' type
  newton vrs str (Exp e1) initialGuess = initialGuess - ((eval vrs (Exp e1)) / (eval vrs (partDiff str (Exp e1))))
  
  -- ^ Performs Newtons Method for a 'var' 'Expr' type
  newton vrs str (Var e1) initialGuess = initialGuess - ((eval vrs (Var e1)) / (eval vrs (partDiff str (Var e1))))

  
  -- ^ Evaluate an 'Add' 'Expr' type
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  
  -- ^ Evaluate a 'Mult' 'Expr' type
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2

  -- ^ Evaluate a 'Div' 'Expr' type  
  eval vrs (Div e1 e2)  = eval vrs e1 / eval vrs e2
  
  -- ^ Evaluate a 'Const' 'Expr' type  
  eval vrs (Const x) = x
  
  -- ^ Evaluate a 'Cos' 'Expr' type  
  eval vrs (Cos x) = cos (eval vrs x)
  
  -- ^ Evaluate a 'Sin' 'Expr' type  
  eval vrs (Sin x) = sin (eval vrs x)
  
  -- ^ Evaluate a 'Ln' 'Expr' type  
  eval vrs (Ln x) = log (eval vrs x)
  
  -- ^ Evaluate a 'Exp' 'Expr' type  
  eval vrs (Exp x) = exp (eval vrs x)
  
  -- ^ Evaluate a 'Exponent' 'Expr' type  
  eval vrs (Exponent base power) = (eval vrs base) ** (eval vrs power)
  
  -- ^ Evaluate a 'Var' 'Expr' type  
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  
  -- ^ Evaluate a 'Exponent' 'Expr' type
  eval vrs (Neg e) = (-1) * (eval vrs e)
  

  {- | All simplification methods for the addition expression
   - Includes all possible pattern matches
   -}
  simplify vrs (Add (Const a) (Const b)) = Const (eval vrs (Add (Const a) (Const b)))
  simplify vrs (Add (Var x) (Const a)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Add (Var x) (Const a)))
                                            Nothing -> Add (Var x) (Const a)
  
  simplify vrs (Add (Const a) (Var x)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Add (Var x) (Const a)))
                                            Nothing -> Add (Var x) (Const a)
  
  simplify vrs (Add (Var x) (Var y)) = case Map.lookup x vrs of
                                            Just v  -> case Map.lookup y vrs of
                                                        Just v   -> Const (eval vrs (Add (Var x) (Var y)))
                                                        Nothing  -> Add (simplify vrs (Var x)) (simplify vrs (Var y))
                                            Nothing -> Add (simplify vrs (Var x)) (simplify vrs (Var y))

  simplify vrs (Add e1 e2) = Add (simplify vrs e1) (simplify vrs e2)


  {- All simplification methods for the multiply expression
   - Includes all possible pattern matches
   -}
  simplify vrs (Mult (Const a) (Const b)) = Const (eval vrs (Mult (Const a) (Const b)))
  simplify vrs (Mult (Var x) (Const a)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Mult (Var x) (Const a)))
                                            Nothing -> Mult (Var x) (Const a)
  simplify vrs (Mult (Const a) (Var x)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Mult (Var x) (Const a)))
                                            Nothing -> Mult (Var x) (Const a)
  simplify vrs (Mult (Var x) (Var y)) = case Map.lookup x vrs of
                                            Just v  -> case Map.lookup y vrs of
                                                        Just v   -> Const (eval vrs (Mult (Var x) (Var y)))
                                                        Nothing  -> Mult (simplify vrs (Var x)) (simplify vrs (Var y))
                                            Nothing -> Mult (simplify vrs (Var x)) (simplify vrs (Var y))
  simplify vrs (Mult e1 e2) = Mult (simplify vrs e1) (simplify vrs e2)
  
  {- All simplification methods for the division expression
   - Includes all possible pattern matches
   -}
  simplify vrs (Div (Const a) (Const b)) = Const (eval vrs (Div (Const a) (Const b)))
  simplify vrs (Div e1 (Const 0)) = Var "INFINITY"
  simplify vrs (Div (Var x) (Const a)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Div (Var x) (Const a)))
                                            Nothing -> Div (Var x) (Const a)
  simplify vrs (Div (Const a) (Var x)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Div (Var x) (Const a)))
                                            Nothing -> Div (Var x) (Const a)
  simplify vrs (Div (Var x) (Var y)) = case Map.lookup x vrs of
                                            Just v  -> case Map.lookup y vrs of
                                                        Just v   -> Const (eval vrs (Div (Var x) (Var y)))
                                                        Nothing  -> Div (simplify vrs (Var x)) (simplify vrs (Var y))
                                            Nothing -> Div (simplify vrs (Var x)) (simplify vrs (Var y))
  simplify vrs (Div e1 e2) = Div (simplify vrs e1) (simplify vrs e2)

  {- All simplification methods for the Sin and Cos expression
   - Includes all possible pattern matches
   -}  
  simplify vrs (Cos (Const a)) = Const (eval vrs (Cos (Const a)))
  simplify vrs (Cos (Var x)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Cos (Var x)))
                                            Nothing -> Cos (Var x)
  simplify vrs (Cos e1) = Cos (simplify vrs e1)
  
  simplify vrs (Sin (Const a)) = Const (eval vrs (Sin (Const a)))
  simplify vrs (Sin (Var x)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Sin (Var x)))
                                            Nothing -> Sin (Var x)
  simplify vrs (Sin e1) = Sin (simplify vrs e1)

  {- All simplification methods for the the exponential expression
   - Includes all possible pattern matches
   -}
  simplify vrs (Exponent (Const a) (Const b)) = Const (eval vrs (Exponent (Const a) (Const b)))
  simplify vrs (Exponent (Var x) (Const a)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Exponent (Var x) (Const a)))
                                            Nothing -> Exponent (Var x) (Const a)
  simplify vrs (Exponent (Const a) (Var x)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Exponent (Var x) (Const a)))
                                            Nothing -> Exponent (Var x) (Const a)
  simplify vrs (Exponent (Var x) (Var y)) = case Map.lookup x vrs of
                                            Just v  -> case Map.lookup y vrs of
                                                        Just v   -> Const (eval vrs (Exponent (Var x) (Var y)))
                                                        Nothing  -> Exponent (simplify vrs (Var x)) (simplify vrs (Var y))
                                            Nothing -> Exponent (simplify vrs (Var x)) (simplify vrs (Var y))
  simplify vrs (Exponent e1 e2) = Exponent (simplify vrs e1) (simplify vrs e2)

  {- Simplification methods for the natural exponent expression
   - Includes all possible pattern matches
   -}
  simplify vrs (Exp (Const a)) = Const (eval vrs (Exp (Const a)))
  simplify vrs (Exp (Var x)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Exp (Var x)))
                                            Nothing -> Exp (Var x)
  simplify vrs (Exp e1) = Exp (simplify vrs e1)

  {- Simplification methods for the natural logarithm expression
   - Includes all possible pattern matches
   -}
  simplify vrs (Ln (Const a)) = Const (eval vrs (Ln (Const a)))
  simplify vrs (Ln (Var x)) = case Map.lookup x vrs of
                                            Just v  -> Const (eval vrs (Ln (Var x)))
                                            Nothing -> Ln (Var x)
  simplify vrs (Ln e1) = Ln (simplify vrs e1)

  {- Simplification methods for the other basic expressions 
   - Includes all possible pattern matches
   -}
  simplify vrs (Const a) = Const a
  simplify vrs (Neg e1) = Mult (Const (-1)) (simplify vrs e1)
  simplify vrs (Var x) = case Map.lookup x vrs of
                           Just v -> Const (eval vrs (Var x))
                           Nothing -> Var x

  {- Partial differentiation methods for all constructors of Expr
   - Includes all possible pattern matches
   -}

  -- ^ differentiate both terms separately
  partDiff str (Add e1 e2) = Add (partDiff str e1) (partDiff str e2)
  
  -- ^ perform product rule for terms that are being multiplied
  partDiff str (Mult e1 e2) = Add (Mult (partDiff str e1) (e2)) (Mult (e1) (partDiff str e2))
  
  -- ^ perform quotient rule for terms that are being divided
  partDiff str (Div e1 e2) = Div (Add (Mult (partDiff str e1) (e2)) (Mult (Neg e1) (partDiff str e2))) (Exponent e2 (Const 2))
  
  -- ^ derivative of a constant is 0
  partDiff str (Const x) = Const 0
  
  -- ^ derivative of cosx is -sinx
  partDiff str (Cos x) = Mult (Neg (Sin x)) (partDiff str x)
  
  -- ^ derivative of sinx is cosx
  partDiff str (Sin x) = Mult (Cos x) (partDiff str x)
  
  -- ^ derivative of ln(x) is (1/x)
  partDiff str (Ln x) = Mult (Div (Const 1) (x)) (partDiff str x)
  
  -- ^ derivative of e^x is e^x * derivative of x 
  partDiff str (Exp x) = Mult (Exp x) (partDiff str x)
  
  -- ^ derivative of a^x is ln(a) * a^x * x'
  partDiff str (Exponent base power) = Mult (Mult (Ln base) (Exponent base power)) (partDiff str power)
  partDiff str (Neg e1) = Mult (Neg (Const 1)) (partDiff str e1)
  partDiff str (Var x) | x == str = (Const 1)
                       | otherwise = (Const 0)
