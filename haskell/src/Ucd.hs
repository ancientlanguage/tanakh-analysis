module Ucd where

import Data.ByteString (ByteString)
import qualified Data.ByteString
import System.FilePath ((</>))

data Version = Version_12_1

unicodeBaseDirectory :: FilePath
unicodeBaseDirectory = "unicode"

ucdFile :: FilePath
ucdFile = "UnicodeData.txt"

getSubPath :: Version -> FilePath
getSubPath Version_12_1 = "ucd-12.1" </> ucdFile

getPath :: Version -> FilePath
getPath version = unicodeBaseDirectory </> getSubPath version

loadUcd :: Version -> IO ByteString
loadUcd version = Data.ByteString.readFile (getPath version)
