{-# LANGUAGE LambdaCase, DeriveFunctor, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Numbers
( Expr
, num
, (+:)
, (-:)
, (*:)
, (/:)
, (^:)
, evalIntegral
, evalFloating
) where

-- Algebras over a functor f
type Algebra f a = f a -> a

-- Functor fixpoints
newtype Fix f = In { out :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show = show . out

-- Catamorphisms
cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . out

-- Algebraic datatype for expressions
data Expr' number expr
  = Number number
  | expr :+ expr
  | expr :- expr
  | expr :* expr
  | expr :/ expr
  | expr :^ expr
  deriving (Show, Functor)
type Expr number = Fix (Expr' number)

-- Smart constructors for expressions
-- (numbers)
num :: n -> Expr n
num = In . Number
-- (and binary operators)
(+:),(-:),(*:),(/:),(^:) :: Expr n -> Expr n -> Expr n
x +: y = In $ x :+ y
x -: y = In $ x :- y
x *: y = In $ x :* y
x /: y = In $ x :/ y
x ^: y = In $ x :^ y

instance Num n => Num (Expr n) where
  fromInteger = num . fromInteger
  (+) = (+:)
  (-) = (-:)
  (*) = (*:)
  abs = error "`abs' not defined for `Expr's"
  signum = error "`signum' not defined for `Expr's"

instance Fractional n => Fractional (Expr n) where
  fromRational = num . fromRational
  (/) = (/:)

instance Floating n => Floating (Expr n) where
  (**) = (^:)
  pi = num pi
  exp = error "`exp' not defined for `Expr's"
  log = error "`log' not defined for `Expr's"
  logBase = error "`logBase' not defined for `Expr's"
  sin = error "`sin' not defined for `Expr's"
  cos = error "`cos' not defined for `Expr's"
  tan = error "`tan' not defined for `Expr's"
  asin = error "`asin' not defined for `Expr's"
  acos = error "`acos' not defined for `Expr's"
  atan = error "`atan' not defined for `Expr's"
  sinh = error "`asinh' not defined for `expr's"
  cosh = error "`acosh' not defined for `expr's"
  tanh = error "`atanh' not defined for `expr's"
  asinh = error "`asinh' not defined for `expr's"
  acosh = error "`acosh' not defined for `expr's"
  atanh = error "`atanh' not defined for `expr's"

-- Evalgebras (made that word up but it works nicely)
evalIntegral' :: Integral n => Algebra (Expr' n) n
evalFloating' :: Floating n => Algebra (Expr' n) n
evalIntegral' = \case
  Number n -> n
  e1 :+ e2 -> e1 + e2
  e1 :- e2 -> e1 - e2
  e1 :* e2 -> e1 * e2
  e1 :/ e2 -> e1 `div` e2
  e1 :^ e2 -> e1 ^ e2
evalFloating' = \case
  Number n -> n
  e1 :+ e2 -> e1 + e2
  e1 :- e2 -> e1 - e2
  e1 :* e2 -> e1 * e2
  e1 :/ e2 -> e1 / e2
  e1 :^ e2 -> e1 ** e2

-- `Eval' functions
evalIntegral :: Integral n => Expr n -> n
evalFloating :: Floating n => Expr n -> n
evalIntegral = cata evalIntegral'
evalFloating = cata evalFloating'

{- Sample expression
test :: Num n => Expr n
test = 9 ^: (5 /: 2)
-}
