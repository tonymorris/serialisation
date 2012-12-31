{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

module Data.Serialisation.Encoder where

import "type-class" Data.Functor.Contravariant
import Data.Functor.Coproduct
import "type-class" Data.Functor.Product
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Semigroup
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Category
import Data.Semigroupoid.Tensor
import Data.Semigroupoid.Disjoint
import Data.Semigroupoid.CombineOut
import Data.Semigroupoid.ChooseOut
import Data.Semigroupoid.Left
import Data.Semigroupoid.Right
import Data.Semigroupoid.Split


data Encoder x y =
  Encoder (y -> x)

instance Semigroup x => Semigroup (Encoder x y) where
  Encoder f <> Encoder g =
    Encoder (\y -> f y <> g y)

instance Monoid x => Monoid (Encoder x y) where
  identity =
    Encoder (\_ -> identity)

instance Contravariant (Encoder x) where
  f -$ Encoder k =
    Encoder (k . f)

instance Semigroupoid Encoder where
  Encoder f . Encoder g =
    Encoder (g . f)

instance Category Encoder where
  id =
    Encoder id

instance Tensor Encoder where
  Encoder k *** Encoder l =
    Encoder (\(a :/\ b) -> k a :/\ l b)

instance Disjoint Encoder where
  Encoder k +++ Encoder l =
    Encoder (\x -> case x of
                     L b -> L (k b)
                     R d -> R (l d))

instance CombineOut Encoder where
  Encoder k >*< Encoder l =
    Encoder (\x -> k x :/\ l x)

instance ChooseOut Encoder where
  Encoder k +-+ Encoder l =
    Encoder (\q -> case q of
                     L a -> k a
                     R b -> l b)

instance Left Encoder where
  left (Encoder k) =
    Encoder (\q -> case q of
                    L b -> L (k b)
                    R c -> R c)

instance Right Encoder where
  right (Encoder k) =
    Encoder (\q -> case q of
                    L b -> L b
                    R c -> R (k c))

instance Split Encoder where
  out =
    Encoder
