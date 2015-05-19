{-# LANGUAGE LambdaCase, DeriveFunctor #-}
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
  deriving Functor
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
test = num 9 ^: (num 5 /: num 2)
-}
