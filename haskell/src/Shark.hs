-- | Description: Basic language implementation
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Shark where

import Control.Lens ((^.))
import qualified Data.Array
import qualified Data.Foldable as Foldable
import Data.Generics.Product (the)
import qualified Data.Monoid as Monoid
import Numeric.Natural (Natural)
import Shark.Types

isValidValue :: ValueNumber -> Size -> Bool
isValidValue (ValueNumber n) (Size s) = n < s

reduceValueBySize :: Size -> ValueNumber -> ValueNumber
reduceValueBySize (Size s) (ValueNumber n) = ValueNumber (n - s)

caseInfoSize :: CaseInfo -> Size
caseInfoSize info =
  let sizeToSum (Size size) = Monoid.Sum size
      sumToSize (Monoid.Sum size) = Size size
  in sumToSize $ Foldable.fold $ fmap sizeToSum $ info ^. the @"sizes"

valueToCase :: CaseInfo -> Value -> Either ValueToCaseError CaseValue
valueToCase info value =
  if value ^. the @Size /= caseInfoSize info
    then Left $ ValueToCaseError ValueToCaseError_SizeMismatch info value
    else valueToCaseUnchecked info value

valueToCaseUnchecked :: CaseInfo -> Value -> Either ValueToCaseError CaseValue
valueToCaseUnchecked info value =
  let sizes :: [Size]
      sizes = Data.Array.elems $ info ^. the @"sizes"

      -- find the case value by repeated subtraction
      build :: [Size] -> CaseIndex -> ValueNumber
        -> Either ValueToCaseError CaseValue
      build [] _ _ = Left $ ValueToCaseError ValueToCaseError_TooLarge info value
      build (s : ss) index num =
        if isValidValue num s
          then Right $ CaseValue info index num
          else build ss (index + 1) (reduceValueBySize s num)
  in case sizes of
      [] -> Left $ ValueToCaseError ValueToCaseError_EmptyCases info value
      _ : _ -> build sizes 0 (value ^. the @ValueNumber)

arrayInfoSize :: ArrayInfo -> Size
arrayInfoSize info =
  (info ^. the @"elementSize")
    ^ (info ^. the @"elementCount" . the @Natural)

valueToArray :: ArrayInfo -> Value -> Either ValueToArrayError ArrayValue
valueToArray info value =
  if value ^. the @Size /= arrayInfoSize info
    then Left $ ValueToArrayError ValueToArrayError_SizeMismatch info value
    else valueToArrayUnchecked info value

valueToArrayUnchecked :: ArrayInfo -> Value
  -> Either ValueToArrayError ArrayValue
valueToArrayUnchecked _info = error "not implemented"
