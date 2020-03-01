module Ucd where

import Data.ByteString (ByteString)
import qualified Data.ByteString
import Data.Word (Word8)
import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as Array
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

newtype ByteCount = ByteCount (UArray Word8 Int)

getNonZeroBytes :: ByteCount -> [(Word8, Int)]
getNonZeroBytes (ByteCount array) = filter (\(_, c) -> c /= 0) $ Array.assocs array

countBytes :: ByteString -> ByteCount
countBytes input =
  let byteList :: [Word8]
      byteList = Data.ByteString.unpack input

      -- the index is the byte value with a count of 1 as the value
      makeIndexed :: Word8 -> (Word8, Int)
      makeIndexed byte = (byte, 1)

      indexedValues :: [(Word8, Int)]
      indexedValues = fmap makeIndexed byteList

      bounds :: (Word8, Word8)
      bounds = (minBound, maxBound)

      array :: UArray Word8 Int
      array = Array.accumArray (+) 0 bounds indexedValues
  in ByteCount array
