{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Control.Monad
import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import qualified Tanakh
import qualified System.Exit

main :: IO ()
main = do
  isSuccess <-
    Hedgehog.checkParallel $
      Hedgehog.Group
        "Tanakh"
        [ ("(natToNormalizedSum . toNat) preserves numeric value", toNatAndBackPreservesValue)
        ]
  Control.Monad.unless isSuccess System.Exit.exitFailure

genSumDepth :: Int -> Int -> Hedgehog.Gen Tanakh.Sum
genSumDepth unitCount _depth | unitCount == 0 = return $ Tanakh.Sum []
genSumDepth _unitCount depth | depth == 0 = return Tanakh.Unit -- might not match unitCount
genSumDepth unitCount depth | depth == 1 = return $ Tanakh.Sum $ take unitCount $ repeat Tanakh.Unit
genSumDepth _unitCount _depth = do
  _listLength <- Hedgehog.Gen.integral @_ @Int $ Hedgehog.Range.linear 1 10
  undefined -- TODO: pick distribution of units across tree of given depth

genSum :: Hedgehog.Gen Tanakh.Sum
genSum = do
  unitCount <- Hedgehog.Gen.integral @_ @Int $ Hedgehog.Range.linear 0 100
  treeDepth <- Hedgehog.Gen.integral @_ @Int $ Hedgehog.Range.linear 0 5
  genSumDepth unitCount treeDepth

toNatAndBackPreservesValue :: Hedgehog.Property
toNatAndBackPreservesValue = Hedgehog.property $ do
  sumData <- Hedgehog.forAll genSum
  (Tanakh.sumToNat sumData) === (Tanakh.sumToNat (Tanakh.natToNormalizedSum (Tanakh.sumToNat sumData)))
