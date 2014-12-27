{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies,TupleSections, InstanceSigs, RankNTypes, DeriveFunctor #-}
module Variadic where

newtype Return a = Return a deriving Functor

class FunSuffix r s where
  type ChangeRtn s c
  fsmap :: (r -> a) -> s -> ChangeRtn s a

instance FunSuffix r (Return r) where
  type ChangeRtn (Return r) c = c
  fsmap f (Return x) = f x

instance FunSuffix r s => FunSuffix r (a -> s) where
  type ChangeRtn (a -> s) c = a -> ChangeRtn s c
  fsmap f = fmap $ fsmap f 

