-- | Description: Exports of type and data constructors
-- from 'Types'.
module Shark.TypeConstructors (module X) where

import Shark.Types as X
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
    ValueToCaseError(ValueToCaseError)
  )
