{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Validated.Validated
  ( Validated (..)
  , Result (..)
  , mkR

  , ValidWhen (..)

  , ValidatedOr
  ) where

import Validated.Predicates

import Data.Bifunctor (second)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import qualified Data.Validation as V

class Validated (a :: Type) where
  type Unvalidated a
    :: Type
  type PredicatesFor a
    :: [Type]
  mk
    :: Unvalidated a
    -> V.Validation (ErrorsFor (PredicatesFor a) (Unvalidated a)) a

newtype Result a
  = Result (V.Validation (ErrorsFor (PredicatesFor a) (Unvalidated a)) a)
  deriving stock Show

mkR
  :: forall a
   . Validated a
  => Unvalidated a
  -> Result a
mkR =
  coerce (mk @a)

newtype ValidWhen (dst :: Type) (satisfies :: ([Type], Type))
  = ValidWhen dst

instance (Coercible src dst, Predicates ps src)
      =>  Validated (ValidWhen dst '(ps, src)) where
  type Unvalidated (ValidWhen dst '(ps, src))
    = src
  type PredicatesFor (ValidWhen dst '(ps, src))
    = ps
  mk x =
    second coerce (testAll @ps @src x)

class (Validated a,
       AllPredicateErrors c (PredicatesFor a) (Unvalidated a))
    => ValidatedOr c a

instance (Validated a,
          AllPredicateErrors c (PredicatesFor a) (Unvalidated a))
      =>  ValidatedOr c a
