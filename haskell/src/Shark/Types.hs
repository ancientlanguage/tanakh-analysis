-- | Description: Exports of type and data constructors
-- from 'TypeDefinitions'.
-- 
-- It is expected to import this unqualified
-- and use generic-lens to get fields in records.
module Shark.Types (module X) where

import Shark.TypeDefinitions as X
  ( Size(Size),
    ValueNumber(ValueNumber),
    Value(Value),
    CaseInfo(CaseInfo),
    CaseIndex(CaseIndex),
    CaseValue(CaseValue),
    ValueToCaseErrorReason
    ( ValueToCaseError_SizeMismatch
    , ValueToCaseError_EmptyCases
    , ValueToCaseError_TooLarge
    ),
    ValueToCaseError(ValueToCaseError),
    ArrayInfo(ArrayInfo),
    ElementCount(ElementCount),
    ArrayValue(ArrayValue)
  )
