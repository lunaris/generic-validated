{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericValidated.Predicates.Types
  ( Predicate (..)

  , ErrorsFor (..)

  , AllPredicateErrors
  , Predicates
  , testAll

  , PredicatesWith
  , errorFor
  ) where

import Data.Bifunctor (second)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~~:) (..))
import qualified Data.Validation as V
import Type.Reflection (Typeable, eqTypeRep, typeRep)

class Typeable p => Predicate (p :: Type) (a :: Type) where
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

errorFor
  :: forall q ps a
   . PredicatesWith q ps a
  => PredicateError q a
  -> ErrorsFor ps a
errorFor
  = errorFor' (Proxy @q)

testAll
  :: Predicates ps a
  => a
  -> V.Validation (ErrorsFor ps a) a
testAll x
  = second fst (testAll' x)

class Predicates (ps :: [Type]) (a :: Type) where
  noErrors
    :: ErrorsFor ps a
  testAll'
    :: a
    -> V.Validation (ErrorsFor ps a) (a, ErrorsFor ps a)

instance Predicates '[] a where
  noErrors =
    NilE
  testAll' x =
    V.Success (x, NilE)

instance (Predicate p a, Predicates ps a)
      =>  Predicates (p ': ps) a where
  noErrors =
    Nothing :> noErrors
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

class (Predicate q a, Predicates ps a) => PredicatesWith q ps a where
  errorFor'
    :: Predicate q a
    => Proxy q
    -> PredicateError q a
    -> ErrorsFor ps a

instance (Predicate q a, Predicates ps a)
      =>  PredicatesWith q (q ': ps) a where
  errorFor' _ e =
    Just e :> noErrors

instance (Predicate p a, PredicatesWith q ps a)
      => PredicatesWith q (p ': ps) a where
  errorFor' prx e =
    Nothing :> errorFor' prx e
