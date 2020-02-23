{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import qualified Control.Monad
import qualified Hedgehog
import qualified Hedgehog.Gen
import qualified Hedgehog.Range
import qualified System.Exit

main :: IO ()
main = do
  isSuccess <-
    Hedgehog.checkParallel $
      Hedgehog.Group
        "Tanakh"
        [ ("stubProperty", stubProperty)
        ]
  Control.Monad.unless isSuccess System.Exit.exitFailure

stubProperty :: Hedgehog.Property
stubProperty = Hedgehog.property $ do
  number <- Hedgehog.forAll $ Hedgehog.Gen.integral @_ @Int $ Hedgehog.Range.linear 1 100
  Hedgehog.assert (number > 0)
