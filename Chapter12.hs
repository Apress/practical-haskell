{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Chapter12 where

data Zero
data Succ n

data Vect n a where
  VNil  :: Vect Zero a
  VCons :: a -> Vect n a -> Vect (Succ n) a
  
headVect :: Vect (Succ n) a -> a
headVect (VCons x _) = x

{-
class Plus x y z | x y -> z, x z -> y
instance Plus Zero x x
instance Plus x y z => Plus (Succ x) y (Succ z)
-}

append :: Vect x a -> Vect y a -> Vect (Plus x y) a
append VNil         ys = ys
append (VCons x xs) ys = VCons x (append xs ys)

type family Plus x y where
  Plus Zero     x = x
  Plus (Succ x) y = Succ (Plus x y)
