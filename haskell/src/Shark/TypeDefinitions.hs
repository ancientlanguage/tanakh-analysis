-- | Description: Type definitions for basic Shark language implementation.
-- It is expected to import the 'Shark.Types' module unqualified and not use this one.
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Shark.TypeDefinitions where

import qualified Data.Array
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- Arrays indexed by 'Natural' numbers
type InternalArray e = Data.Array.Array Natural e

-- Represents the max size of a number.
-- So if you have a number of this size, then it is in the range 0 to less-than this number.
--
-- If size = 0, then you can't have any numbers of this size, since Naturals have to be >= 0, and
-- and the size means that the number has to be < 0, which is impossible.
--
-- If size is 1, then you can one possible value, which is 0, since 0 >= 0 and 0 < 1.
--
-- If size is 2, then there are two possible values
-- (and you can see the size "means" the number of possible values)
-- 0, 1 because:
-- * 0 =<  0  < 2 and
-- * 0 =<  1  < 2
newtype Size = Size Natural
  deriving newtype (Eq, Ord, Num)
  deriving stock (Generic)

newtype ValueNumber = ValueNumber Natural
  deriving newtype (Eq, Ord, Num)
  deriving stock (Generic)

-- A Value is a number within a certain range (zero to less than size)
-- Requirement is that  0 =<  value  < size
data Value = Value
  { size :: Size
  , number :: ValueNumber
  }
  deriving (Generic)

-- An encoding is a bijection between numbers from 0 (inclusive) to less-than 'size'.
--
-- We represent it as a list, where the index is the encoding,
-- and the number in the list is the value.
--
-- Example, if size = 2, then we have 2 values: 0 and 1.
-- However, maybe we want the number '0' (say, on the computer itself) to represent our '1',
-- and '1' on the computer to represent our '0'.
--
-- You think of encodings like a table
-- | computer number | our interpreted number |
-- | 0               | 1                      |
-- | 1               | 0                      |
--
-- In our Haskell value, this would be represented as the list '[1, 0]'
-- because you can imagine the 0-based index as being the left column in the above (the computer number).
-- So you can think of '[1, 0]' as being a table of
-- [ (0, 1)
-- , (1, 0)
-- ]
--
-- For size 3, we can choose 3! possible encodings.
-- [0,1,2]
-- [0,2,1]
-- [1,0,2]
-- [1,2,0]
-- [2,0,1]
-- [2,1,0]
data Encoding = Encoding
  { size :: Size
  , values :: InternalArray Natural
    -- ^ Must be an array of size 'encodingSize'
    -- and each element must be unique,
    -- and each element must be in the range 0 <= elem < 'encodingSize'
  }
  deriving (Generic)

data CaseInfo = CaseInfo
  { sizes :: InternalArray Size
  }
  deriving (Generic)

newtype CaseIndex = CaseIndex Natural
  deriving newtype (Eq, Ord, Num)
  deriving stock (Generic)

data CaseValue = CaseValue
  { info :: CaseInfo
  , caseIndex :: CaseIndex
  , caseValue :: ValueNumber
  }
  deriving (Generic)

-- data Product = Product { sizeOfItems :: [Size] } -- implicit in the list is the number of items

data ValueToCaseErrorReason
  = ValueToCaseError_SizeMismatch
  | ValueToCaseError_EmptyCases
  | ValueToCaseError_TooLarge
  deriving (Generic)

data ValueToCaseError
  = ValueToCaseError
    { reason :: ValueToCaseErrorReason
    , caseInfo :: CaseInfo
    , value :: Value
    }
  deriving (Generic)

newtype ElementCount = ElementCount Natural
  deriving newtype (Eq, Ord, Num)
  deriving stock (Generic)

data ArrayInfo = ArrayInfo
  { elementSize :: Size
  , elementCount :: ElementCount
  }
  deriving Generic

data ArrayValue = ArrayValue
  { info :: ArrayInfo
  , values :: InternalArray ValueNumber
  }
  deriving Generic

data ValueToArrayErrorReason
  = ValueToArrayError_SizeMismatch
  | ValueToArrayError_NotDivisibleByElementSize ValueNumber

data ValueToArrayError = ValueToArrayError
  { reason :: ValueToArrayErrorReason
  , arrayInfo :: ArrayInfo
  , value :: Value
  }
  deriving Generic
