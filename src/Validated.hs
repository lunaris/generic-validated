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

import qualified Data.Barbie as B
import Data.Bifunctor (second)
import Data.Coerce (Coercible, coerce)
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

class Predicate (p :: Type) (a :: Type) where
  type PredicateError p a :: Type
  test :: a -> V.Validation (PredicateError p a) a

data SizeLessThan (n :: Nat)

data SizeLessThanError
  = SizeTooGreat
  deriving stock (Generic, Show)

instance (KnownNat n, Foldable f, a ~ f x)
      =>  Predicate (SizeLessThan n) a where
  type PredicateError (SizeLessThan n) a
    = SizeLessThanError
  test x
    | length x >= numVal @n = V.Failure SizeTooGreat
    | otherwise             = V.Success x

numVal :: forall n a. (KnownNat n, Num a) => a
numVal =
  fromInteger (natVal (Proxy @n))

newtype ValidWhen (dst :: Type) (satisfies :: ([Type], Type))
  = ValidWhen dst

class Validated (a :: Type) where
  type Unvalidated a
    :: Type
  type PredicatesFor a
    :: [Type]
  mk
    :: Unvalidated a
    -> V.Validation (ErrorsFor (PredicatesFor a) (Unvalidated a)) a

instance (Coercible src dst, Predicates ps src)
      =>  Validated (ValidWhen dst '(ps, src)) where
  type Unvalidated (ValidWhen dst '(ps, src))
    = src
  type PredicatesFor (ValidWhen dst '(ps, src))
    = ps
  mk x =
    second coerce (testAll @ps @src x)

mkR
  :: forall a
   . Validated a
  => Unvalidated a
  -> Result a
mkR =
  coerce (mk @a)

newtype Result a
  = Result (V.Validation (ErrorsFor (PredicatesFor a) (Unvalidated a)) a)
  deriving stock Show

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

data ErrorsFor :: [Type] -> Type -> Type where
  NilE :: ErrorsFor '[] a
  (:>) :: !(Maybe (PredicateError p a))
       -> !(ErrorsFor ps a)
       -> ErrorsFor (p ': ps) a

infixr 5 :>

type family AllPredicateErrors
  (c :: Type -> Constraint)
  (ps :: [Type])
  (a :: Type)
    :: Constraint where
  AllPredicateErrors _ '[] _
    = ()
  AllPredicateErrors c (p ': ps) a
    = (c (PredicateError p a), AllPredicateErrors c ps a)

deriving instance AllPredicateErrors Show ps a
              =>  Show (ErrorsFor ps a)

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

instance (Predicate p a, Predicates ps a) => Predicates (p ': ps) a where
  testAll' x =
    case testAll' @ps x of
      V.Failure es ->
        case test @p x of
          V.Failure e ->
            V.Failure (Just e :> es)
          V.Success y ->
            V.Failure (Nothing :> es)
      V.Success (z, es) ->
        case test @p x of
          V.Failure e ->
            V.Failure (Just e :> es)
          V.Success _ ->
            V.Success (z, Nothing :> es)
