{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Validated.Codes
  ( GenericCodedSum
  , sequenceErrorCodes
  ) where

import Validated.Predicates
import Validated.Validated

import qualified Data.Aeson as Ae
import qualified Data.Barbie as B
import Data.Bifunctor (first)
import Data.Functor.Const (Const (..))
import Data.Functor.Product (Product (..))
import qualified Data.Generic.HKD as HKD
import qualified Data.Generic.HKD.Labels as HKD.Labels
import qualified Data.Generic.HKD.Types as HKD.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Validation as V
import Generics.Deriving.ConNames (ConNames, conNameOf)
import GHC.Generics (Generic, Rep)

sequenceErrorCodes
  :: forall a
   . ( Generic a
     , HKD.Labels.Label a
     , HKD.Construct (V.Validation (HM.HashMap String [String])) a
     , HKD.Types.ProductBC (HKD.HKD a)
     , HKD.Types.GAllB (ValidatedOr GenericCodedSum) (Rep a)
     )
  => HKD.HKD a Result
  -> V.Validation (HM.HashMap String [String]) a
sequenceErrorCodes x =
  let ls = HKD.label @a
      es = B.bmapC @(ValidatedOr GenericCodedSum) errorCodesList x
      p  = B.bzipWith errorCodesMap ls es
  in  HKD.construct p

errorCodesMap
  :: Const String a
  -> V.Validation [String] a
  -> V.Validation (HM.HashMap String [String]) a
errorCodesMap (Const k)
  = first (HM.singleton k)

class (Generic a, ConNames (Rep a)) => GenericCodedSum a
instance (Generic a, ConNames (Rep a)) => GenericCodedSum a

errorCodesList
  :: forall a
   . ValidatedOr GenericCodedSum a
  => Result a
  -> V.Validation [String] a
errorCodesList (Result x) =
  case x of
    V.Failure es -> V.Failure (errorCodesList' es)
    V.Success x  -> V.Success x

errorCodesList'
  :: AllPredicateErrors GenericCodedSum ps a
  => ErrorsFor ps a
  -> [String]
errorCodesList' = \case
  NilE    -> []
  e :> es -> maybe id ((:) . conNameOf) e (errorCodesList' es)
