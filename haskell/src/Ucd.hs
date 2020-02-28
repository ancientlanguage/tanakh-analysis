module Ucd where

import System.FilePath ((</>))

data Version = Version_12_1

unicodeBaseDirectory :: FilePath
unicodeBaseDirectory = "unicode"

ucdFile :: FilePath
ucdFile = "UnicodeData.txt"

getPath :: Version -> FilePath
getPath Version_12_1 = "ucd-12.1" </> ucdFile
