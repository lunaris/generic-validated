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

import Validated.Codes
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
