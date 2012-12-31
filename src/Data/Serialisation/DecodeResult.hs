{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

module Data.Serialisation.DecodeResult where

import Data.Functor.Applicative
import "type-class" Data.Functor.Apply
import "type-class" Data.Functor.Alt
import "type-class" Data.Functor.Bind
import "type-class" Data.Functor.Extend
import Data.Functor.Functor
import Data.Functor.Monad
import Data.Functor.Foldable
import Data.Functor.Traversable
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Semigroup
import Data.Serialisation.History

data DecodeResult e h y =
  DecodeResultFailure e (History h)
  | DecodeResultSuccess y

instance (Semigroup e, Semigroup y) => Semigroup (DecodeResult e h y) where
  DecodeResultSuccess x <> DecodeResultSuccess y =
    DecodeResultSuccess (x <> y)
  DecodeResultSuccess x <> DecodeResultFailure _ _ =
    DecodeResultSuccess x
  DecodeResultFailure _ _ <> DecodeResultSuccess x =
    DecodeResultSuccess x
  DecodeResultFailure e h <> DecodeResultFailure f i =
    DecodeResultFailure (e <> f) (h <> i)

instance (Monoid e, Monoid y) => Monoid (DecodeResult e h y) where
  identity =
    DecodeResultFailure identity identity

instance Functor (DecodeResult e h) where
  _ $ DecodeResultFailure e h =
    DecodeResultFailure e h
  f $ DecodeResultSuccess y =
    DecodeResultSuccess (f y)

instance Apply (DecodeResult e h) where
  DecodeResultSuccess f <*> DecodeResultSuccess y =
    DecodeResultSuccess (f y)
  DecodeResultFailure e h <*> DecodeResultSuccess _ =
    DecodeResultFailure e h
  DecodeResultFailure e h <*> DecodeResultFailure _ _ =
    DecodeResultFailure e h
  DecodeResultSuccess _ <*> DecodeResultFailure e h =
    DecodeResultFailure e h

instance Bind (DecodeResult e h) where
  _ =<< DecodeResultFailure e h =
    DecodeResultFailure e h
  f =<< DecodeResultSuccess y =
    f y

instance Applicative (DecodeResult e h) where
  pure =
    DecodeResultSuccess

instance Monad (DecodeResult e h) where

instance Alt (DecodeResult e h) where
  DecodeResultSuccess y <|> DecodeResultSuccess _ =
    DecodeResultSuccess y
  DecodeResultFailure _ _ <|> r@(DecodeResultSuccess _) =
    r
  DecodeResultFailure _ _ <|> DecodeResultFailure e h =
    DecodeResultFailure e h
  r@(DecodeResultSuccess _) <|> DecodeResultFailure _ _ =
    r

instance Extend (DecodeResult e h) where
  f `extend` r@(DecodeResultSuccess _) =
    DecodeResultSuccess (f r)
  _ `extend` DecodeResultFailure e h =
    DecodeResultFailure e h

instance Foldable (DecodeResult e h) where
  foldMap _ (DecodeResultFailure _ _) =
    identity
  foldMap f (DecodeResultSuccess y) =
    f y

instance Traversable (DecodeResult e h) where
  traverse _ (DecodeResultFailure e h) =
    pure (DecodeResultFailure e h)
  traverse f (DecodeResultSuccess y) =
    DecodeResultSuccess $ f y
