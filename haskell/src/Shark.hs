-- | Description: Basic language implementation
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Shark where

import qualified Data.Array
import qualified Data.Foldable as Foldable
import qualified Data.Monoid as Monoid
import Shark.TypeConstructors
import qualified Shark.Types as Shark

isValidValue :: ValueNumber -> Size -> Bool
isValidValue (ValueNumber n) (Size s) = n < s

reduceValueBySize :: Size -> ValueNumber -> ValueNumber
reduceValueBySize (Size s) (ValueNumber n) = ValueNumber (n - s)

caseInfoSize :: CaseInfo -> Size
caseInfoSize info =
  let sizeToSum (Size size) = Monoid.Sum size
      sumToSize (Monoid.Sum size) = Size size
  in sumToSize $ Foldable.fold $ fmap sizeToSum $ Shark.sizeOfCases info

valueToCase :: CaseInfo -> Value -> Either ValueToCaseError CaseValue
valueToCase info value =
  let actualValueSize = Shark.valueSize value
      expectedCaseSize = caseInfoSize info
  in if expectedCaseSize /= actualValueSize
      then Left $ ValueToCaseError ValueToCaseError_SizeMismatch info value
      else valueToCaseUnchecked info value

valueToCaseUnchecked :: CaseInfo -> Value -> Either ValueToCaseError CaseValue
valueToCaseUnchecked info value =
  let sizes :: [Size]
      sizes = Data.Array.elems $ Shark.sizeOfCases info

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
      _ : _ -> build sizes 0 (Shark.valueNumber value)
