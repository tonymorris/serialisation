{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

module Data.Serialisation.HCursor where

import Data.Functor.Applicative
import "type-class" Data.Functor.Apply
import "type-class" Data.Functor.Bind
import "type-class" Data.Functor.Extend
import Data.Functor.Functor
import Data.Functor.Monad
import Data.Functor.Comonad
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Semigroup
import Data.Semigroupoid.Semigroupoid
import Data.Serialisation.Cursor
import Data.Serialisation.History

data HCursor h x t =
  HCursor (History h) (Cursor x t)

instance Functor (HCursor h x) where
  f $ HCursor h c =
    HCursor h (f $ c)

instance Extend (HCursor h x) where
  f `extend` i@(HCursor h (Cursor x _)) =
    HCursor h (Cursor x (f i))

instance Comonad (HCursor h x) where
  extract (HCursor _ c) =
    extract c

instance Semigroup x => Apply (HCursor h x) where
  HCursor h f <*> HCursor h' t =
    HCursor (h <> h') (f <*> t)

instance Monoid x => Applicative (HCursor h x) where
  pure =
    HCursor identity . pure

instance Semigroup x => Bind (HCursor h x) where
  f =<< HCursor h (Cursor x t) =
    let HCursor h' (Cursor x' t') = f t
    in HCursor (h <> h') (Cursor (x <> x') t')

instance Monoid x => Monad (HCursor h x) where
