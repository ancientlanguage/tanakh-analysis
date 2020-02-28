import qualified Control.Monad
import qualified Properties
import qualified System.Exit

main :: IO ()
main = do
  propertiesPass <- Properties.check
  let allChecksPass = and [propertiesPass]
  Control.Monad.unless allChecksPass System.Exit.exitFailure
