{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
module Chapter14Generics where

import GHC.Generics
import GHC.TypeLits
import Data.Proxy

class GetAll t a where
  getall :: Proxy t -> a -> [t]
  default getall :: (Generic a, GGetAll t (Rep a)) => Proxy t -> a -> [t]
  getall p = ggetall p . from

instance {-# OVERLAPS #-} GetAll t t where
  getall p x = [x]
instance {-# OVERLAPPABLE #-} GetAll t s where
  getall p x = []

instance GetAll a [a]

data Tree a = Node a | Branch (Tree a) (Tree a)
            deriving (Show, Eq, Generic)
instance GetAll a (Tree a)

class GGetAll t (f :: * -> *) where
  ggetall :: Proxy t -> f x -> [t]

instance (GGetAll t f, GGetAll t g) => GGetAll t (f :+: g) where
  ggetall p (L1 x) = ggetall p x
  ggetall p (R1 y) = ggetall p y

instance (GGetAll t f) => GGetAll t (M1 v i f) where
  ggetall p (M1 x) = ggetall p x

instance (GGetAll t f, GGetAll t g) => GGetAll t (f :*: g) where
  ggetall p (x :*: y) = ggetall p x ++ ggetall p y

instance GGetAll t U1 where
  ggetall p U1 = []

instance (GetAll t s) => GGetAll t (Rec0 s) where
  ggetall p (K1 x) = getall p x