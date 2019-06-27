{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
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

module GenericValidated.Predicates.Types
  ( Predicate (..)

  , ErrorsFor (..)

  , AllPredicateErrors
  , Predicates
  , testAll
  ) where

import Data.Bifunctor (second)
import Data.Kind (Constraint, Type)
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
