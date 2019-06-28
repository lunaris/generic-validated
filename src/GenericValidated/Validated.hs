{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module GenericValidated.Validated
  ( Validated (..)
  , Result (..)
  , mkR
  , maybeMkR

  , ValidWhen (..)

  , ValidatedOr
  ) where

import GenericValidated.Predicates

import qualified Data.Aeson as Ae
import Data.Bifunctor (second)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Constraint, Type)
import qualified Data.Validation as V
import GHC.Generics (Generic)

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

maybeMkR
  :: forall a ps otherPs
   . ( Validated a
     , PredicatesFor a ~ ps
     , PredicatesWith Present ps (Unvalidated a)
     )
  => Maybe (Unvalidated a)
  -> Result a
maybeMkR = \case
  Nothing ->
    Result $ V.Failure $
      errorFor @Present DataMissing
  Just x ->
    mkR x

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

newtype AlwaysValid a
  = AlwaysValid { getAlwaysValid :: a }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Ae.FromJSON, Ae.ToJSON)
  deriving Validated
    via (AlwaysValid a `ValidWhen`
      '( '[]
       , a
       )
    )

newtype Required a
  = Required { getRequired :: a }
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (Ae.FromJSON, Ae.ToJSON)
  deriving Validated
    via (Required a `ValidWhen`
      '( '[ Present
          ]
       , a
       )
    )
