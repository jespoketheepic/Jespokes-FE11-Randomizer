{-# LANGUAGE NamedFieldPuns #-}
module Lib
    ( action
    ) where

import System.IO
import qualified System.Directory as Dir
import qualified System.FilePath as FP
import qualified Data.ByteString as BS
import qualified System.Process as Pro
import qualified Control.Exception as Ex
import qualified System.Random as RNG
import qualified Data.List as L
import DatabaseParse
import DisposParse
import Settings
import GeneralUtility
import Types
import Randomize
import ApplyChanges
import UpdateRawData
import FixedEdits

action :: IO ()
action = do
  --- Directory stuff
  workingDirectory <- getWorkingDirectory
  let databaseDirectory = workingDirectory ++ FP.pathSeparator:"DSLazy" ++ FP.pathSeparator:"NDS_UNPACK" ++ FP.pathSeparator:"data" ++ FP.pathSeparator:"data" ++ FP.pathSeparator:"database"
  let disposDirectory = workingDirectory ++ FP.pathSeparator:"DSLazy" ++ FP.pathSeparator:"NDS_UNPACK" ++ FP.pathSeparator:"data" ++ FP.pathSeparator:"dispos"

  -- List of all disposss
  disposDirectoryContents <- Dir.listDirectory disposDirectory
  let disposDirectoryContentsFull = map ((disposDirectory ++ [FP.pathSeparator]) ++) disposDirectoryContents

  --- Decompress ---
  decompress workingDirectory databaseDirectory
  mapM_ (decompress workingDirectory) disposDirectoryContentsFull

  -- Read files
  database <- BS.readFile databaseDirectory
  disposFiles <- mapM BS.readFile disposDirectoryContentsFull
  let disposTuples = zip disposFiles disposDirectoryContentsFull
  let databaseStruct = parseDatabase database
  let disposStruct = parseDisposFiles disposTuples

  --- Gather Settings ---
  (settings, order) <- modeSelectInit databaseStruct
  --- Fixed edits ---
  let databaseStruct2 = fixedPreRandoEdits databaseStruct
  let databaseStruct3 = noteChrClass databaseStruct2 disposStruct

  --- Randomize ---
  randG <- RNG.getStdGen
  let (databaseStruct4, disposStruct2) = randomize databaseStruct3 disposStruct randG settings
  prfReport databaseStruct4 workingDirectory
  --- Meme Mart ---

  --- Fixed post rando edits --
  let disposStruct3 = fixedPostRandoDispEdits disposStruct2

  --- Apply Changes ---
  let databaseStruct5 = updateRawDataDB databaseStruct4
  let disposStruct4 = updateRawDataDisp disposStruct3
  applyChangesDB databaseStruct5 database databaseDirectory
  applyChangesAllDispos disposStruct4
  logChrs workingDirectory databaseStruct5

  --- Compress ---
  compress workingDirectory databaseDirectory
  mapM_ (compress workingDirectory) disposDirectoryContentsFull
  hFlush stdout

  --- Print datastructures
  --print (dbCharacters databaseStruct4)
  --putStrLn "\n"
  --print (dbClasses databaseStruct4)
  --putStrLn "\n"
  --print (dbItems databaseStruct4)
  --putStrLn "\n"
  --print disposStruct3
  putStrLn "Ok, done!"

  --let dispentries = concatMap (\x@DisposFile{entries} -> entries) disposStruct2
  --let uniquecharinvs = L.groupBy (dispGroup) ( L.sortBy (dispord) dispentries)
  --mapM_  (\x -> appendFile (workingDirectory ++ "\\DisposCount.txt") (show x ++ "\n")) uniquecharinvs
  --writeFile (workingDirectory ++ "\\Item_nr_chart.txt") (concatMap (\x -> snd (iID x) ++ " = " ++ show (fromIntegral(itemNr x)) ++ "\n") (items databaseStruct))

--dispord :: DisposEntry -> DisposEntry -> Ordering
--dispord dis1 dis2
--  | chrID dis1 < chrID dis2 = LT
--  | chrID dis1 > chrID dis2 = GT
--  | otherwise = (inventory dis1) `compare` (inventory dis2)
--
--dispGroup :: DisposEntry -> DisposEntry -> Bool
--dispGroup dis1 dis2 = chrID dis1 == chrID dis2 && (inventory dis1) == (inventory dis2)

compressor :: Directory -> Directory -> Char -> IO()
compressor working target mode = do
  already <- Ex.try (Pro.callProcess (working ++ FP.pathSeparator:"CompressLZ77") (target:[[mode]])) :: IO (Either Ex.SomeException ())
  case already of
    Right a -> print "ok"
    Left e -> print "also ok"

decompress :: Directory -> Directory -> IO()
decompress working target = compressor working target 'd'

compress :: Directory -> Directory -> IO()
compress working target = compressor working target 'c'