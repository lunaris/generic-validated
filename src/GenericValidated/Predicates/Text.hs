{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericValidated.Predicates.Text
  ( LengthLessThan
  , LengthLessThanError (..)

  , LengthGreaterThan
  , LengthGreaterThanError (..)

  , LengthLessThanOrEqual
  , LengthLessThanOrEqualError (..)

  , LengthGreaterThanOrEqual
  , LengthGreaterThanOrEqualError (..)

  , MatchesRegex
  , MatchesRegexError (..)

  , DoesNotMatchRegex
  , DoesNotMatchRegexError (..)
  ) where

import GenericValidated.Predicates.Types
import GenericValidated.Predicates.Util

import Data.Proxy (Proxy (..))
import qualified Data.Text as Tx
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, symbolVal)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

-------------------------------------------------------------------------------
--  Predicates over strict `Text` values
-------------------------------------------------------------------------------

data LengthGreaterThan (n :: Nat)

data LengthGreaterThanError
  = LengthTooLittleOrEqual
  deriving stock (Generic, Show)

instance KnownNat n => Predicate (LengthGreaterThan n) Tx.Text where
  type PredicateError (LengthGreaterThan n) Tx.Text
    = LengthGreaterThanError
  test =
    LengthTooLittleOrEqual `when` \x -> Tx.length x <= numVal @n

data LengthLessThan (n :: Nat)

data LengthLessThanError
  = LengthTooGreatOrEqual
  deriving stock (Generic, Show)

instance KnownNat n => Predicate (LengthLessThan n) Tx.Text where
  type PredicateError (LengthLessThan n) Tx.Text
    = LengthLessThanError
  test =
    LengthTooGreatOrEqual `when` \x -> Tx.length x >= numVal @n

data LengthGreaterThanOrEqual (n :: Nat)

data LengthGreaterThanOrEqualError
  = LengthTooLittle
  deriving stock (Generic, Show)

instance KnownNat n => Predicate (LengthGreaterThanOrEqual n) Tx.Text where
  type PredicateError (LengthGreaterThanOrEqual n) Tx.Text
    = LengthGreaterThanOrEqualError
  test =
    LengthTooLittle `when` \x -> Tx.length x < numVal @n

data LengthLessThanOrEqual (n :: Nat)

data LengthLessThanOrEqualError
  = LengthTooGreat
  deriving stock (Generic, Show)

instance KnownNat n => Predicate (LengthLessThanOrEqual n) Tx.Text where
  type PredicateError (LengthLessThanOrEqual n) Tx.Text
    = LengthLessThanOrEqualError
  test =
    LengthTooGreat `when` \x -> Tx.length x > numVal @n

data MatchesRegex (regex :: Symbol)

data MatchesRegexError
  = RegexNotMatched
  deriving stock (Generic, Show)

instance KnownSymbol regex => Predicate (MatchesRegex regex) Tx.Text where
  type PredicateError (MatchesRegex regex) Tx.Text
    = MatchesRegexError
  test =
    RegexNotMatched `when` \x -> not (x =~ textVal @regex)

data DoesNotMatchRegex (regex :: Symbol)

data DoesNotMatchRegexError
  = RegexMatched
  deriving stock (Generic, Show)

instance KnownSymbol regex => Predicate (DoesNotMatchRegex regex) Tx.Text where
  type PredicateError (DoesNotMatchRegex regex) Tx.Text
    = DoesNotMatchRegexError
  test =
    RegexMatched `when` \x -> x =~ textVal @regex

textVal :: forall s. KnownSymbol s => Tx.Text
textVal =
  Tx.pack (symbolVal (Proxy @s))
