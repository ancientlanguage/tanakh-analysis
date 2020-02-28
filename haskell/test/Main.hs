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

genSum :: Hedgehog.Gen Tanakh.Sum
genSum = do
  depthOption <- Hedgehog.Gen.integral @_ @Int $ Hedgehog.Range.linear 0 2
  case depthOption of
    0 -> return $ Tanakh.Sum []
    1 -> return $ Tanakh.Sum [Tanakh.Unit]
    _ -> do
      listLength <- Hedgehog.Gen.integral @_ @Int $ Hedgehog.Range.linear 0 10
      children <- traverse (const genSum) [1..listLength]
      return $ Tanakh.Sum children

toNatAndBackPreservesValue :: Hedgehog.Property
toNatAndBackPreservesValue = Hedgehog.property $ do
  sumData <- Hedgehog.forAll genSum
  (Tanakh.sumToNat sumData) === (Tanakh.sumToNat (Tanakh.natToNormalizedSum (Tanakh.sumToNat sumData)))
