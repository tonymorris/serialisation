{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

module Data.Serialisation.History where

import Data.Functor.Applicative
import "type-class" Data.Functor.Apply
import "type-class" Data.Functor.Bind
import "type-class" Data.Functor.Extend
import Data.Functor.Functor
import Data.Functor.Monad
import Data.Semigroupoid.Monoid
import Data.Semigroupoid.Semigroup
import Prelude(concatMap, map, (++))

newtype History a =
  History [a]

instance Semigroup (History a) where
  History a <> History b =
    History (a ++ b)

instance Monoid (History a) where
  identity =
    History []

instance Functor History where
  f $ History a =
    History (map f a)

instance Apply History where
  History f <*> History a =
    History (concatMap (\f' -> map f' a) f)

instance Bind History where
  f =<< History a =
    History (concatMap (\x -> let History h = f x in h) a)

instance Applicative History where
  pure a =
    History [a]

instance Monad History where

instance Extend History where
  extend _ (History []) =
    History []
  extend f (q@(History (_:t))) =
    History (f q : let History h = extend f (History t) in h)