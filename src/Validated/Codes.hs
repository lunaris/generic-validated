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
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Tx
import qualified Data.Validation as V
import Generics.Deriving.ConNames (ConNames, conNameOf)
import GHC.Generics (Generic, Rep)

sequenceErrorCodes
  :: forall a
   . ( Generic a
     , HKD.Labels.Label a
     , HKD.Construct (V.Validation (HM.HashMap Tx.Text [Tx.Text])) a
     , B.ProductB (HKD.HKD a)
     , B.ConstraintsB (HKD.HKD a)
     , B.AllB (ValidatedOr GenericCodedSum) (HKD.HKD a)
     )
  => HKD.HKD a Result
  -> V.Validation (HM.HashMap Tx.Text [Tx.Text]) a
sequenceErrorCodes x =
  let ls = HKD.label @a
      es = B.bmapC @(ValidatedOr GenericCodedSum) errorCodesList x
      p  = B.bzipWith errorCodesMap ls es
  in  HKD.construct p

errorCodesMap
  :: Const String a
  -> V.Validation [Tx.Text] a
  -> V.Validation (HM.HashMap Tx.Text [Tx.Text]) a
errorCodesMap (Const k)
  = first (HM.singleton (Tx.pack k))

class (Generic a, ConNames (Rep a)) => GenericCodedSum a
instance (Generic a, ConNames (Rep a)) => GenericCodedSum a

errorCodesList
  :: forall a
   . ValidatedOr GenericCodedSum a
  => Result a
  -> V.Validation [Tx.Text] a
errorCodesList (Result x) =
  case x of
    V.Failure es -> V.Failure (errorCodesList' es)
    V.Success x  -> V.Success x

errorCodesList'
  :: AllPredicateErrors GenericCodedSum ps a
  => ErrorsFor ps a
  -> [Tx.Text]
errorCodesList' = \case
  NilE    -> []
  e :> es -> maybe id ((:) . Tx.pack . conNameOf) e (errorCodesList' es)
