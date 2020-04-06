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

-- Sum type
-- where a value is exactly one of the types in a list
-- (but not more).
--
-- It represents an exclusive "OR" logically.
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

-- Natural number as a type "Max Size"
-- Natural number as a value, should be < max size
-- defined by our type
--
-- Physically on a computer, we start with voltage
-- above or below a certain threshold to get 0 or 1
-- 
-- Then we copy each of those bits to make an array.
-- "8 bit".
--
-- After a Natural number, we want do define everything
-- else in terms of those two number
-- (the type size, and the value number).
--
-- If we allow ourselves to name two natural numbers,
-- then we can a primitive of an array.
-- Where the numbers are:
-- * element size
-- * length of the array
--
-- By starting number is 10.
-- Everything you do needs to be less than 10.
-- Then we can talk about "8 bits".
-- Array: element size 2 and length 8
-- and both of those number are less than 10.
-- 
-- 32-bits or 64-bits
-- 31-bits or 16 or 15 bits, historically 7-bits
--
-- We can represent the sum type / cases with two concepts
-- (max size and arrays).
--
-- We could use two natural numbers for our CaseInfo.
--   elemSize is some natural number
--   length is some natural number
--
-- Say we have an 8-bit system
-- We can define a CaseInfo / SumType as two 8-bit number.
-- [ bbbb bbbb ] [ bbbb bbbb ]
--   ^^^^^^^^^ interpret this part as the element size 
--                 ^^^^^^^^^ interpret as the array length
--
-- Example
-- [ 22 ] [ 1 ]
-- This can represent Hebrew characters
-- Each character there is only one option (the letter)
-- And we have 22 of them.
--
-- But Hebrew has final forms, so if we want to 
-- distinguish each character by final form, then
-- we need two options for each letter: final or not.
-- [ 22 ] [ 2 ]
--          ^^^ value 0 means medial form
--              value 1 means final form
--
-- For a CaseValue, we need to know how big that value
-- can get, because has to have a max size.
-- Here the max number is elemSize * arrayLength
-- (adding up all of the element values).
-- [ 22 ] [ 1 ] = max size is 22 * 1 = 22
-- [ 22 ] [ 2 ] = max size of value is 22 * 2 = 44
--
-- For a product, the representation is the same,
-- but the interpretation is different.
-- [ 22 ] [ 5 ]
-- That means I have 5 Hebrew letters, which is a short word.
--
-- But the product value not elemSize * arrayLength,
-- instead it's elemSize ^ arrayLength.
-- [ 22 ] [ 5 ], max size = 22 ^ 5 = 5,153,632
--
