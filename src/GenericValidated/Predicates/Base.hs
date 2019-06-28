{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericValidated.Predicates.Base
  ( Present
  , PresentError (..)

  , LessThan
  , LessThanError (..)

  , GreaterThan
  , GreaterThanError (..)

  , LessThanOrEqual
  , LessThanOrEqualError (..)

  , GreaterThanOrEqual
  , GreaterThanOrEqualError (..)

  , LengthLessThan
  , LengthLessThanError (..)

  , LengthGreaterThan
  , LengthGreaterThanError (..)

  , LengthLessThanOrEqual
  , LengthLessThanOrEqualError (..)

  , LengthGreaterThanOrEqual
  , LengthGreaterThanOrEqualError (..)
  ) where

import GenericValidated.Predicates.Types
import GenericValidated.Predicates.Util

import Data.Kind (Constraint, Type)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat)
import qualified Data.Validation as V

-------------------------------------------------------------------------------
--  Library-supplied predicates
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--  Data which is required
-------------------------------------------------------------------------------

data Present

data PresentError
  = DataMissing
  deriving stock (Generic, Show)

instance Predicate Present a where
  type PredicateError Present a
    = PresentError
  test =
    V.Success

-------------------------------------------------------------------------------
--  Predicates over `Num`s
-------------------------------------------------------------------------------

data GreaterThan (n :: Nat)

data GreaterThanError
  = TooLittleOrEqual
  deriving stock (Generic, Show)

instance (KnownNat n, Num a, Ord a)
      =>  Predicate (GreaterThan n) a where
  type PredicateError (GreaterThan n) a
    = GreaterThanError
  test =
    TooLittleOrEqual `when` \x -> x <= numVal @n

data LessThan (n :: Nat)

data LessThanError
  = TooGreatOrEqual
  deriving stock (Generic, Show)

instance (KnownNat n, Num a, Ord a)
      =>  Predicate (LessThan n) a where
  type PredicateError (LessThan n) a
    = LessThanError
  test =
    TooGreatOrEqual `when` \x -> x >= numVal @n

data GreaterThanOrEqual (n :: Nat)

data GreaterThanOrEqualError
  = TooLittle
  deriving stock (Generic, Show)

instance (KnownNat n, Num a, Ord a)
      =>  Predicate (GreaterThanOrEqual n) a where
  type PredicateError (GreaterThanOrEqual n) a
    = GreaterThanOrEqualError
  test =
    TooLittle `when` \x -> x < numVal @n

data LessThanOrEqual (n :: Nat)

data LessThanOrEqualError
  = TooGreat
  deriving stock (Generic, Show)

instance (KnownNat n, Num a, Ord a)
      =>  Predicate (LessThanOrEqual n) a where
  type PredicateError (LessThanOrEqual n) a
    = LessThanOrEqualError
  test =
    TooGreat `when` \x -> x > numVal @n

-------------------------------------------------------------------------------
--  Predicates over `Foldable`s
-------------------------------------------------------------------------------

data LengthGreaterThan (n :: Nat)

data LengthGreaterThanError
  = LengthTooLittleOrEqual
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (LengthGreaterThan n) a where
  type PredicateError (LengthGreaterThan n) a
    = LengthGreaterThanError
  test =
    LengthTooLittleOrEqual `when` \x -> length x <= numVal @n

data LengthLessThan (n :: Nat)

data LengthLessThanError
  = LengthTooGreatOrEqual
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (LengthLessThan n) a where
  type PredicateError (LengthLessThan n) a
    = LengthLessThanError
  test =
    LengthTooGreatOrEqual `when` \x -> length x >= numVal @n

data LengthGreaterThanOrEqual (n :: Nat)

data LengthGreaterThanOrEqualError
  = LengthTooLittle
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (LengthGreaterThanOrEqual n) a where
  type PredicateError (LengthGreaterThanOrEqual n) a
    = LengthGreaterThanOrEqualError
  test =
    LengthTooLittle `when` \x -> length x < numVal @n

data LengthLessThanOrEqual (n :: Nat)

data LengthLessThanOrEqualError
  = LengthTooGreat
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (LengthLessThanOrEqual n) a where
  type PredicateError (LengthLessThanOrEqual n) a
    = LengthLessThanOrEqualError
  test =
    LengthTooGreat `when` \x -> length x > numVal @n
