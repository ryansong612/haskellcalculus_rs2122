module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = Val . fromInteger
  negate      = UnApp Neg
  (+)         = BinApp Add
  (*)         = BinApp Mul
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined


instance Fractional Exp where
  fromRational = Val . fromRational
  (/)          = BinApp Div
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

-- pre: binding must be in environment
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp variable environment = fromJust(lookup variable environment)

unTo :: UnOp -> String
unTo Neg = "-"
unTo Sin = "sin"
unTo Cos = "cos"
unTo Log = "log"

binTo :: BinOp -> String
binTo Add = "+"
binTo Mul = "*"
binTo Div = "/"

showExp :: Exp -> String
showExp (Val a)            = show a
showExp (Id a)             = a
showExp (UnApp ops vs)     = unTo ops ++ "(" ++ showExp vs ++ ")"
showExp (BinApp ops v1 v2) = "(" ++ showExp v1 ++ binTo ops ++ showExp v2 ++ ")"

-- type Env = [(String,Double)]
binOps = [
  (Add, (+)),
  (Mul, (*)),
  (Div, (/))
  ]

unOps = [
  (Neg, negate),
  (Sin, sin),
  (Cos, cos),
  (Log, log)
  ]

eval :: Exp -> Env -> Double
eval (Val x) _           = x
eval (Id x) env          = lookUp x env
eval (BinApp op x y) env = lookUp op binOps (eval x env) (eval y env)
eval (UnApp op x) env    = lookUp op unOps (eval x env)


diff :: Exp -> String -> Exp
diff (Val x) _ = 0
diff (Id x) var
  | x == var  = 1
  | otherwise = 0   -- y != x

diff (BinApp op e1 e2) var 
  | op == Add = (diff e1 var) + (diff e2 var)
  | op == Mul = (e1 * (diff e2 var)) + ((diff e1 var) * e2)
  | op == Div = numerator / denominator
  where
    numerator    = ((diff e1 var) * e2) - (e1 * (diff e2 var))
    denominator  = e2 * e2

diff (UnApp op exp) var
  | op == Neg = negate chain
  | op == Sin = (cos exp) * chain
  | op == Cos = negate ((sin exp) * chain)
  | op == Log = chain / exp
  where
    chain = diff exp var

maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp x n
  = sum (zipWith3 (\x y z -> (x * z) / y) series facts xs)
  where
    series = map (`eval` [("x", 0.0)]) (take n (iterate (`diff` "x") exp))
    facts = 1 : scanl (*) 1 [2..]
    xs = take n (iterate (* x) 1)
-- take n $ iterate (diff exp) var
---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
