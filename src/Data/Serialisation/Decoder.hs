{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

module Data.Serialisation.Decoder where

import Data.Functor.Applicative
import "type-class" Data.Functor.Apply
import "type-class" Data.Functor.Bind
import Data.Functor.Functor
import Data.Functor.Monad
import Data.Functor.Coproduct
import "type-class" Data.Functor.Product
import Data.Semigroupoid.Arrow
import Data.Semigroupoid.Category
import Data.Semigroupoid.ChooseIn
import Data.Semigroupoid.CombineIn
import Data.Semigroupoid.Disjoint
import Data.Semigroupoid.First
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Second
import Data.Semigroupoid.Semigroup
import Data.Semigroupoid.Semigroupoid
import Data.Semigroupoid.Tensor
import Data.Serialisation.Cursor
import Data.Serialisation.DecodeResult
import Data.Serialisation.HCursor

newtype Decoder e h t x y =
  Decoder (HCursor h x t -> DecodeResult e h y)

instance (Semigroup e, Semigroup y) => Semigroup (Decoder e h t x y) where
  Decoder k <> Decoder l =
    Decoder (\c -> k c <> l c)

instance (Monoid e, Monoid y) => Monoid (Decoder e h t x y) where
  identity =
    Decoder (\_ -> identity)

instance Functor (Decoder e h t x) where
  f $ Decoder k =
    Decoder (\c -> f $ k c)

instance Apply (Decoder e h t x) where
  Decoder f <*> Decoder a =
    Decoder (\c -> f c <*> a c)

instance Bind (Decoder e h t x) where
  f =<< Decoder k =
    Decoder (\c -> (\o -> let Decoder l = f o in l c) =<< k c)

instance Applicative (Decoder e h t x) where
  pure a =
    Decoder (\_ -> pure a)

instance Monad (Decoder e h t x) where

instance Semigroupoid (Decoder e h t) where
  Decoder k . Decoder l =
    Decoder (\c@(HCursor h (Cursor _ t)) -> case l c of
                     DecodeResultSuccess a -> k (HCursor h (Cursor a t))
                     DecodeResultFailure e h' -> DecodeResultFailure e h')

instance Category (Decoder e h t) where
  id =
    Decoder (\(HCursor _ (Cursor x _)) -> DecodeResultSuccess x)

instance Tensor (Decoder e h t) where
  Decoder k *** Decoder l =
    Decoder (\(HCursor h (Cursor (a :/\ c) t)) ->
      (\b -> (\d -> b :/\ d) $ l (HCursor h (Cursor c t))) =<< k (HCursor h (Cursor a t)))

instance Disjoint (Decoder e h t) where
  Decoder k +++ Decoder l =
    Decoder (\(HCursor h (Cursor x t)) -> case x of
                                            L a -> L $ k (HCursor h (Cursor a t))
                                            R b -> R $ l (HCursor h (Cursor b t)))

instance CombineIn (Decoder e h t) where
  Decoder k *-* Decoder l =
    Decoder (\c -> (\u -> (\v -> u :/\ v) $ l c) =<< k c)

instance ChooseIn (Decoder e h t) where
  Decoder k >+< Decoder l =
    Decoder (\(HCursor h (Cursor x t)) -> case x of
                                            L a -> k (HCursor h (Cursor a t))
                                            R b -> l (HCursor h (Cursor b t)))

instance First (Decoder e h t) where
  first (Decoder k) =
    Decoder (\(HCursor h (Cursor (a :/\ c) t)) -> (\b -> b :/\ c) $ k (HCursor h (Cursor a t)))

instance Second (Decoder e h t) where
  second (Decoder k) =
    Decoder (\(HCursor h (Cursor (c :/\ a) t)) -> (\b -> c :/\ b) $ k (HCursor h (Cursor a t)))

instance Arrow (Decoder e h t) where
  into f =
    Decoder (\(HCursor _ (Cursor a _)) -> DecodeResultSuccess (f a))