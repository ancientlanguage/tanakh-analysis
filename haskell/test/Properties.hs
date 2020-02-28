{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Properties where

import Hedgehog ((===))
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import qualified Ideas

check :: IO Bool
check = do
  Hedgehog.checkParallel $
    Hedgehog.Group
      "Ideas"
      [ ("(natToNormalizedSum . toNat) preserves numeric value", toNatAndBackPreservesValue)
      ]

genSum :: Hedgehog.Gen Ideas.Sum
genSum = do
  depthOption <- Hedgehog.Gen.integral @_ @Int $ Hedgehog.Range.linear 0 2
  case depthOption of
    0 -> return $ Ideas.Sum []
    1 -> return $ Ideas.Sum [Ideas.Unit]
    _ -> do
      listLength <- Hedgehog.Gen.integral @_ @Int $ Hedgehog.Range.linear 0 5
      children <- traverse (const genSum) [1..listLength]
      return $ Ideas.Sum children

toNatAndBackPreservesValue :: Hedgehog.Property
toNatAndBackPreservesValue = Hedgehog.property $ do
  sumData <- Hedgehog.forAll genSum
  (Ideas.sumToNat sumData) === (Ideas.sumToNat (Ideas.natToNormalizedSum (Ideas.sumToNat sumData)))
