module DisposParse (
  parseDisposFiles,
  DisposEntry(..),
  disChrID
) where

import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import Data.Word
import Types

import UnsafeTestkit

--- Constants ---
headerSize = 0x20
startIndexLoc = 0x24
checkIndex = 0x4A
checkValue = 0x64
entrySize = 0x50


--- Getters ---
disChrID :: DisposEntry -> Int
disChrID de = fromIntegral(toInteger(fst (chrID de)) + toInteger(snd (chrID de)) * 256)

disClassID :: DisposEntry -> Word8
disClassID = classID


--- Parsing ---
parseDisposFiles :: [(BS.ByteString, FilePath)] -> [DisposFile]
parseDisposFiles = map parseDisposFileInit

parseDisposFileInit :: (BS.ByteString, FilePath) -> DisposFile
parseDisposFileInit (file, filePath) = DisposFile{dispfileID=reverse (take 3 (reverse filePath)), filePath=filePath, entryStartOffset=fromIntegral (BS.index file startIndexLoc) + headerSize
  , entries=parseDispos (BS.drop (fromIntegral (BS.index file startIndexLoc) + headerSize) file)}

parseDispos :: BS.ByteString -> [DisposEntry]
parseDispos file
  | entryCheck file = DisposEntry {chrID= (BS.index file 0, BS.index file 1), classID=BS.index file 2
      , loadCrds=(BS.index file 4, BS.index file 5), endCrds=(BS.index file 6, BS.index file 7)
      , level=(BS.index file 9, BS.index file 10), inventory=getWeapons file, rawData_disp=take entrySize (BS.unpack file)}
      : parseDispos (BS.drop entrySize file)
  | otherwise = []

--- I use the pattern 64,00 to check validity
--- The redundant bracket warnings are wrong!
entryCheck :: BS.ByteString -> Bool
entryCheck file = (BS.index file checkIndex == checkValue)
  && ((BS.index file (checkIndex +1)) == 0x00)

getWeapons :: BS.ByteString -> [(Word8, Word8)]
getWeapons file = (BS.index file 16,BS.index file 17):(BS.index file 18,BS.index file 19):(BS.index file 20,BS.index file 21):[(BS.index file 22, BS.index file 23)]


{-# ANN entryCheck "HLint: ignore Redundant bracket" #-}