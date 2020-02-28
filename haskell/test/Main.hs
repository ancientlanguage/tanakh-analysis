{-# LANGUAGE TypeApplications #-}

import qualified Control.Exception
import qualified Control.Monad
import qualified Properties
import qualified System.Exit
import qualified Ucd

main :: IO ()
main = do
  results <- sequence [Properties.check, ucdTests]
  let allChecksPass = and results
  Control.Monad.unless allChecksPass System.Exit.exitFailure

trueIfNoException :: IO a -> IO Bool
trueIfNoException action = do
  eResult <- Control.Exception.try @Control.Exception.SomeException action
  case eResult of
    Left err -> do
      print err
      return False
    Right _ -> return True

ucdTests :: IO Bool
ucdTests = do
  print (Ucd.getPath Ucd.Version_12_1)
  ucd12 <- trueIfNoException $ Ucd.loadUcd Ucd.Version_12_1
  return ucd12
