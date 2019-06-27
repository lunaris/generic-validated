{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module GenericValidated.Predicates.Util
  ( when
  , numVal
  ) where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownNat, natVal)
import qualified Data.Validation as V

-------------------------------------------------------------------------------
--  Predicate utility functions
-------------------------------------------------------------------------------

when :: e -> (a -> Bool) -> a -> V.Validation e a
when e p x =
  if p x then V.Failure e else V.Success x

numVal :: forall n a. (KnownNat n, Num a) => a
numVal =
  fromInteger (natVal (Proxy @n))
