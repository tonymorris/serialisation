{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

module Data.Serialisation.Cursor where

import Data.Functor.Applicative
import "type-class" Data.Functor.Apply
import "type-class" Data.Functor.Bind
import Data.Functor.Comonad
import "type-class" Data.Functor.Extend
import Data.Functor.Functor
import Data.Functor.Monad
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Semigroup

data Cursor x t =
  Cursor x t

instance Functor (Cursor x) where
  f $ Cursor x t =
    Cursor x (f t)

instance Extend (Cursor x) where
  f `extend` c@(Cursor x _) =
    Cursor x (f c)

instance Comonad (Cursor x) where
  extract (Cursor _ t) =
    t

instance Semigroup x => Apply (Cursor x) where
  Cursor x f <*> Cursor x' t =
    Cursor (x <> x') (f t)

instance Monoid x => Applicative (Cursor x) where
  pure =
    Cursor identity

instance Semigroup x => Bind (Cursor x) where
  f =<< Cursor x t =
    let Cursor x' u = f t
    in Cursor (x <> x') u

instance Monoid x => Monad (Cursor x) where
