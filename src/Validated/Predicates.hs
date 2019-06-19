{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Validated.Predicates
  ( Predicate (..)

  , ErrorsFor (..)

  , AllPredicateErrors
  , Predicates
  , testAll

  , SizeLessThan
  , SizeLessThanError (..)

  , SizeGreaterThan
  , SizeGreaterThanError (..)

  , SizeLessThanOrEqual
  , SizeLessThanOrEqualError (..)

  , SizeGreaterThanOrEqual
  , SizeGreaterThanOrEqualError (..)
  ) where

import Data.Bifunctor (second)
import Data.Proxy (Proxy (..))
import Data.Kind (Constraint, Type)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, Nat, natVal)
import qualified Data.Validation as V

class Predicate (p :: Type) (a :: Type) where
  type PredicateError p a :: Type
  test :: a -> V.Validation (PredicateError p a) a

data ErrorsFor :: [Type] -> Type -> Type where
  NilE :: ErrorsFor '[] a
  (:>) :: !(Maybe (PredicateError p a))
       -> !(ErrorsFor ps a)
       -> ErrorsFor (p ': ps) a

deriving instance AllPredicateErrors Show ps a
              =>  Show (ErrorsFor ps a)

type family AllPredicateErrors
  (c :: Type -> Constraint)
  (ps :: [Type])
  (a :: Type)
    :: Constraint where
  AllPredicateErrors _ '[] _
    = ()
  AllPredicateErrors c (p ': ps) a
    = (c (PredicateError p a), AllPredicateErrors c ps a)

infixr 5 :>

testAll
  :: Predicates ps a
  => a
  -> V.Validation (ErrorsFor ps a) a
testAll x
  = second fst (testAll' x)

class Predicates (ps :: [Type]) (a :: Type) where
  testAll' :: a -> V.Validation (ErrorsFor ps a) (a, ErrorsFor ps a)

instance Predicates '[] a where
  testAll' x =
    V.Success (x, NilE)

instance (Predicate p a, Predicates ps a)
      =>  Predicates (p ': ps) a where
  testAll' x =
    case testAll' @ps x of
      V.Failure es ->
        case test @p x of
          V.Failure e -> V.Failure (Just e :> es)
          V.Success y -> V.Failure (Nothing :> es)
      V.Success (z, es) ->
        case test @p x of
          V.Failure e -> V.Failure (Just e :> es)
          V.Success _ -> V.Success (z, Nothing :> es)

-------------------------------------------------------------------------------
--  Library-supplied predicates
-------------------------------------------------------------------------------

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

data SizeGreaterThan (n :: Nat)

data SizeGreaterThanError
  = SizeTooLittleOrEqual
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (SizeGreaterThan n) a where
  type PredicateError (SizeGreaterThan n) a
    = SizeGreaterThanError
  test =
    SizeTooLittleOrEqual `when` \x -> length x <= numVal @n

data SizeLessThan (n :: Nat)

data SizeLessThanError
  = SizeTooGreatOrEqual
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (SizeLessThan n) a where
  type PredicateError (SizeLessThan n) a
    = SizeLessThanError
  test =
    SizeTooGreatOrEqual `when` \x -> length x >= numVal @n

data SizeGreaterThanOrEqual (n :: Nat)

data SizeGreaterThanOrEqualError
  = SizeTooLittle
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (SizeGreaterThanOrEqual n) a where
  type PredicateError (SizeGreaterThanOrEqual n) a
    = SizeGreaterThanOrEqualError
  test =
    SizeTooLittle `when` \x -> length x < numVal @n

data SizeLessThanOrEqual (n :: Nat)

data SizeLessThanOrEqualError
  = SizeTooGreat
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (SizeLessThanOrEqual n) a where
  type PredicateError (SizeLessThanOrEqual n) a
    = SizeLessThanOrEqualError
  test =
    SizeTooGreat `when` \x -> length x > numVal @n

-------------------------------------------------------------------------------
--  Utility functions
-------------------------------------------------------------------------------

when :: e -> (a -> Bool) -> a -> V.Validation e a
when e p x =
  if p x then V.Failure e else V.Success x

numVal :: forall n a. (KnownNat n, Num a) => a
numVal =
  fromInteger (natVal (Proxy @n))
