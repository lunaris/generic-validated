{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Validated where

import Validated.Predicates
import Validated.Validated

import qualified Data.Barbie as B
import Data.Functor.Const (Const (..))
import qualified Data.Generic.HKD as HKD
import Data.Kind (Constraint, Type)
import Generics.Deriving.ConNames (ConNames, conNameOf)
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Data.Proxy (Proxy (..))
import qualified Data.Validation as V

newtype Forename
  = Forename String
  deriving stock (Eq, Show)
  deriving Validated
    via (Forename `ValidWhen`
      '( '[ SizeLessThan 5
          , SizeLessThan 10
          ]
       , String
       )
    )

newtype Surname
  = Surname String
  deriving stock (Eq, Show)
  deriving Validated
    via (Forename `ValidWhen`
      '( '[ SizeLessThan 5
          , SizeLessThan 10
          ]
       , String
       )
    )

data PersonalDetails
  = PersonalDetails
      { forename :: !Forename
      , surname  :: !Surname
      }

  deriving stock (Eq, Generic, Show)

-- |Build a record made of validated bits using HKD and the `Validated`
--  trickery above/below.
ex1 =
  HKD.build @PersonalDetails
    (mkR @Forename "Wolfgang Amadeus")
    (mkR @Surname "Mozart")


-- |Use generic trickery and `barbies` to turn that generic record of
--  all-the-errors into lists of strings suitable for turning into error
--  codes.
ex2 :: HKD.HKD PersonalDetails (Const (Maybe [String]))
ex2 =
  B.bmapC @GenericallyValid foo ex1

-- |Pair those lists of strings with their field names, ready for serialisation
--  as JSON objects that communicate per-field error codes, for free!
ex3 =
  B.bprod (HKD.label ex2) ex2

class (Generic a, ConNames (Rep a)) => GenericSum a
instance (Generic a, ConNames (Rep a)) => GenericSum a

class (Validated a,
       AllPredicateErrors GenericSum (PredicatesFor a) (Unvalidated a))
    => GenericallyValid a

instance (Validated a,
          AllPredicateErrors GenericSum (PredicatesFor a) (Unvalidated a))
      =>  GenericallyValid a

foo :: forall a. GenericallyValid a => Result a -> Const (Maybe [String]) a
foo (Result x) =
  Const $ case x of
    V.Failure es -> Just (urk es)
    V.Success x  -> Nothing

urk :: AllPredicateErrors GenericSum ps a => ErrorsFor ps a -> [String]
urk = \case
  NilE ->
    []
  e :> es ->
    maybe id ((:) . conNameOf) e (urk es)
