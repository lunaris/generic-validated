# generic-validated

This is a library for doing `Validation` using `deriving via` and GHC
`Generic`s.

## Error codes

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import GHC.Generics (Generic)
import qualified Data.Generic.HKD as HKD
import qualified GenericValidated as GV

newtype Forename
  = Forename String
  deriving stock (Eq, Show)
  deriving GV.Validated
    via (Forename `GV.ValidWhen`
      '( '[ GV.LengthLessThan 5
          , GV.LengthLessThan 10
          ]
       , String
       )
    )

newtype Surname
  = Surname String
  deriving stock (Eq, Show)
  deriving GV.Validated
    via (Forename `GV.ValidWhen`
      '( '[ GV.LengthLessThan 5
          , GV.LengthLessThan 10
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

ex1 =
  GV.sequenceErrorCodes $
    HKD.build @PersonalDetails
      (GV.mkR @Forename "Wolfgang Amadeus")
      (GV.mkR @Surname "Mozart")
```
