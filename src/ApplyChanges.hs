{-# LANGUAGE NamedFieldPuns #-}
module ApplyChanges (
  applyChangesDB,
  applyChangesAllDispos,
  logChrs
) where

import Types
import GeneralUtility
import qualified Data.ByteString as BS
import Data.Word
import DatabaseConstants

import UnsafeTestkit

applyChangesDB :: DatabaseStruct -> BS.ByteString -> FilePath -> IO ()
applyChangesDB db filecontent filepath =
  let filelength = BS.length filecontent
      mask = dbContentMask filelength
      fileUpdates = dbTablesOnlyFile db filelength
      newFileContent = BS.pack(zipWithMask_errorOnMismatchedInput (BS.unpack filecontent) fileUpdates mask)
    in BS.writeFile filepath newFileContent


applyChangesAllDispos :: [DisposFile] -> IO ()
applyChangesAllDispos = mapM_ applyChangesDispos

applyChangesDispos :: DisposFile -> IO ()
applyChangesDispos dispFile@DisposFile{filePath=filePath, entryStartOffset=entryStartOffset, entries=entries} = do
  ogFile <- BS.readFile filePath
  let filelength = BS.length ogFile
      maskedUpdates = dispContentMask entries entryStartOffset filelength
      newFileContent = BS.pack(zipWithMaskTup_errorOnMismatchedInput (BS.unpack ogFile) maskedUpdates)
    in BS.writeFile filePath newFileContent

dbContentMask :: Int -> [Bool]
dbContentMask filelength =
      replicate headerSize False
   ++ replicate chr_TableSize True
   ++ replicate chr_class_TableGap False
   ++ replicate class_TableSize True
   ++ replicate class_item_TableGap False
   ++ replicate item_TableSize True
   ++ replicate item_EOF_TableGap False
   where item_EOF_TableGap = filelength - item_TableEnd

dbTablesOnlyFile :: DatabaseStruct -> Int -> [Word8]
dbTablesOnlyFile DatabaseStruct{characters=chrs, classes=clss, items=items} filelength =
      replicate headerSize 0
   ++ reproduceTable chrs
   ++ replicate chr_class_TableGap 0
   ++ reproduceTable clss
   ++ replicate class_item_TableGap 0
   ++ reproduceTable items
   ++ replicate item_EOF_TableGap 0
   where item_EOF_TableGap = filelength - item_TableEnd


dispContentMask :: [DisposEntry] -> Int -> Int -> [(Word8, Bool)]
dispContentMask disposss entryStartOffset filelength =
     replicate entryStartOffset (0, False)
  ++ concatMap (\disp -> map (\x -> (x, True)) (rawData disp)) disposss
  ++ replicate (filelength - (entryStartOffset + disposEntrySize * length disposss)) (0, False)


reproduceTable :: TableEntry a => [a] -> [Word8]
reproduceTable = concatMap rawData

logChrs :: Directory -> DatabaseStruct -> IO ()
logChrs dir db@DatabaseStruct{characters} = do mapM_ (\chr -> appendFile (dir ++ "\\randomizer log.txt") (logentry chr)) (filter (\x -> chrNr x < 0x3B) characters)

logentry :: Character -> String
logentry chr = (snd $ pID chr) ++ " " ++ (case (chrClass chr) of {Nothing -> "None"; Just cls -> snd $ jID cls}) ++ ":\n" ++ "         /  HP / STR / MAG / SKL / SPD / LCK / DEF / RES\n" ++ "Growths: " ++ "/ " ++ (concatMap (\x -> show x ++ "% / ") (chrGrowths chr)) ++ "\n\n"





{-# ANN module "HLint: ignore Use tuple-section" #-}
{-# ANN dispContentMask "HLint: ignore Avoid lambda" #-}