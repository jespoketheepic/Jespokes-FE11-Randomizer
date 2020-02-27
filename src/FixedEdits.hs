{-# LANGUAGE NamedFieldPuns #-}
module FixedEdits where

import Types
import Data.Word
import Data.List
import Data.Maybe
import System.IO
import Table
import GeneralUtility

import UnsafeTestkit

fixedPreRandoEdits :: DatabaseStruct -> DatabaseStruct
fixedPreRandoEdits db@DatabaseStruct{classes} = db{classes=falconKnightInPool classes}

fixedPostRandoDispEdits :: [DisposFile] -> [DisposFile]
fixedPostRandoDispEdits dispFs = prfPreservation $ saveWhitewings dispFs


falconKnightInPool :: [Class] -> [Class]
falconKnightInPool classes = take 3 classes ++ (classes !! 3){reclassSet=6} : drop 4 classes

saveWhitewings :: [DisposFile] -> [DisposFile]
saveWhitewings dispFs = let
  ch14 = find_w_error (\dispF -> (dispfileID dispF) == "014") dispFs "ch14"
  ch18 = find_w_error (\dispF -> (dispfileID dispF) == "018") dispFs "ch18"
  catria = find_w_error (\e -> chrID e == (0x2B, 0)) (entries ch14) "catria"
  palla = find_w_error (\e -> chrID e == (0x2A, 0)) (entries ch14) "palla"
  est = find_w_error (\e -> chrID e == (0x2F, 0)) (entries ch18) "est"
  ch14_i = findIndex_w_error (\dispF -> (dispfileID dispF) == "014") dispFs ""
  ch18_i = findIndex_w_error (\dispF -> (dispfileID dispF) == "018") dispFs ""
  catria_i = findIndex_w_error (\e -> chrID e == (0x2B, 0)) (entries ch14) ""
  palla_i = findIndex_w_error (\e -> chrID e == (0x2A, 0)) (entries ch14) ""
  est_i = findIndex_w_error (\e -> chrID e == (0x2F, 0)) (entries ch18) ""

  catria' = catria{endCrds=(2,14), loadCrds=(1,14)}
  palla' = palla{endCrds=(2,15), loadCrds=(1,15)}
  est' = est{endCrds=(12,22)}
  ch14' = ch14{entries=replaceAtIndex (entries ch14) catria' catria_i}
  ch14'' = ch14'{entries=replaceAtIndex (entries ch14') palla' palla_i}
  ch18' = ch18{entries=replaceAtIndex (entries ch18) catria' catria_i}

  in replaceAtIndex (replaceAtIndex dispFs ch14'' ch14_i) ch18' ch18_i

noteChrClass :: DatabaseStruct -> [DisposFile] -> DatabaseStruct
noteChrClass db@DatabaseStruct{characters, classes} dispFs = db{characters=(map (noteChrClass2 dispFs classes) (filter (\x -> chrNr x < 0x3A) characters)) ++ (filter (\x -> chrNr x > 0x39) characters)}  -- Fuck Ymir i guess

noteChrClass2 :: [DisposFile] -> [Class] -> Character -> Character
noteChrClass2 dispFs classes chr@Character{chrNr} = chr{chrClass=Just (noteChrClass3 (find_w_error (\dispF -> mapChrNrToChapter chrNr == dispfileID dispF) dispFs "dispF") classes (fromIntegral chrNr))}

noteChrClass3 :: DisposFile -> [Class] -> Word8 -> Class
noteChrClass3 dispF classes chrNr = find_w_error (\cls -> noteChrClass4 dispF chrNr == classNr cls) classes "cls"

noteChrClass4 :: DisposFile -> Word8 -> Word8
noteChrClass4 dispF@DisposFile{entries} chrNr = classID (find_w_error (\e -> fst (chrID e) == chrNr) entries ("e " ++ show chrNr))

prfPreservation :: [DisposFile] -> [DisposFile]
prfPreservation dispFs = let
  ch1 = find_w_error (\dispF -> "100" == take 3 (reverse (filePath dispF))) dispFs "ch1"
  ch19 = find_w_error (\dispF -> "910" == take 3 (reverse (filePath dispF))) dispFs "ch19"
  ch4 = find_w_error (\dispF -> "400" == take 3 (reverse (filePath dispF))) dispFs "ch4"
  ch11 = find_w_error (\dispF -> "110" == take 3 (reverse (filePath dispF))) dispFs "ch11"

  remainingDisps = filter (\d -> d/=ch1 && d/=ch4 && d/=ch11 && d/=ch19) dispFs

  marth = find_w_error (\x -> 0x00 == fst (chrID x)) (entries ch1) "marth"
  caeda = find_w_error (\x -> 0x01 == fst (chrID x)) (entries ch1) "caeda"
  merric = find_w_error (\x -> 0x11 == fst (chrID x)) (entries ch4) "merric"
  linde = find_w_error (\x -> 0x21 == fst (chrID x)) (entries ch11) "linde"
  tiki = find_w_error (\x -> 0x30 == fst (chrID x)) (entries ch19) "tiki"
  marth_i = findIndex_w_error (\x -> 0x00 == fst (chrID x)) (entries ch1) "marth_i"
  caeda_i = findIndex_w_error (\x -> 0x01 == fst (chrID x)) (entries ch1) "caeda_i"
  merric_i = findIndex_w_error (\x -> 0x11 == fst (chrID x)) (entries ch4) "merric_i"
  linde_i = findIndex_w_error (\x -> 0x21 == fst (chrID x)) (entries ch11) "linde_i"
  tiki_i = findIndex_w_error (\x -> 0x30 == fst (chrID x)) (entries ch19) "tiki_i"

  marth2 = marth{inventory= take 2 (inventory marth) ++ (0x0A,0) : drop 3 (inventory marth)}
  caeda2 = caeda{inventory= take 1 (inventory caeda) ++ (0x16,0) : drop 2 (inventory caeda)}
  merric2 = merric{inventory= take 1 (inventory merric) ++ (0x3C,0) : drop 2 (inventory merric)}
  linde2 = linde{inventory= take 1 (inventory linde) ++ (0x3D,0) : drop 2 (inventory linde)}
  tiki2 = tiki{inventory= take 1 (inventory tiki) ++ [(0x33,0),(0x31,0)] ++ drop 3 (inventory tiki)}

  ch1' = ch1 {entries=marth2 : caeda2 : drop (caeda_i+1) (entries ch1)}
  ch4' = ch4 {entries=take merric_i (entries ch4) ++ merric2 : drop (merric_i+1) (entries ch4)}
  ch11' = ch11 {entries=take linde_i (entries ch11) ++ linde2 : drop (linde_i+1) (entries ch11)}
  ch19' = ch19 {entries=take tiki_i (entries ch19) ++ tiki2 : drop (tiki_i+1) (entries ch19)}

  in ch1' :  ch4' : ch11' : ch19' : remainingDisps

markInvalidClass :: Int -> Bool
markInvalidClass 0x01 = False
markInvalidClass 0x07 = False
markInvalidClass 0x2B = False
markInvalidClass 0x2C = False
markInvalidClass 0x2D = False
markInvalidClass 0x2E = False
markInvalidClass 0x2F = False
markInvalidClass x = True

markDontGive :: Word8 -> Bool
markDontGive 0x09 = False
markDontGive 0x0C = False
markDontGive 0x0D = False
markDontGive 0x0E = False
markDontGive 0x15 = False
markDontGive 0x30 = False
markDontGive 0x32 = False
markDontGive 0x3D = False
markDontGive 0x46 = False
markDontGive 0x47 = False
markDontGive x = True

prfReport :: DatabaseStruct -> Directory -> IO ()
prfReport db@DatabaseStruct{characters} dir = do
  let rapier = find (\chr -> prf chr == (0x04,0)) characters
  writeFile (dir ++ "\\Prf_Log.txt") (case rapier of {Nothing -> "Nobody"; Just chr -> snd (pID chr)} ++ " can wield the Rapier.\n")
  let wingspear = find (\chr -> prf chr == (0x29,0)) characters
  appendFile (dir ++ "\\Prf_Log.txt") (case wingspear of {Nothing -> "Nobody"; Just chr -> snd (pID chr)} ++ " can wield the Wing Spear.\n")
  let excalibur = find (\chr -> prf chr == (0x80,0)) characters
  appendFile (dir ++ "\\Prf_Log.txt") (case excalibur of {Nothing -> "Nobody"; Just chr -> snd (pID chr)} ++ " can wield the Excalubur from E rank.\n")
  let aura = find (\chr -> prf chr == (0x01,0x01)) characters
  appendFile (dir ++ "\\Prf_Log.txt") (case aura of {Nothing -> "Nobody"; Just chr -> snd (pID chr)} ++ " can wield the Aura from E rank.\n")
  let divine = find (\chr -> prf chr == (0x01,0x02)) characters
  appendFile (dir ++ "\\Prf_Log.txt") (case divine of {Nothing -> "Nobody"; Just chr -> snd (pID chr)} ++ " can wield the Divinestone.\n")
  let earth = find (\chr -> prf chr == (0x10,0)) characters
  appendFile (dir ++ "\\Prf_Log.txt") (case earth of {Nothing -> "Nobody"; Just chr -> snd (pID chr)} ++ " can wield the Earthstone.\n")
  let hammerne = find (\chr -> prf chr == (0x41,0)) characters
  appendFile (dir ++ "\\Prf_Log.txt") (case hammerne of {Nothing -> "Nobody"; Just chr -> snd (pID chr)} ++ " can wield the Hammerne staff.\n")
  let aum = find (\chr -> prf chr == (0x09,0)) characters
  appendFile (dir ++ "\\Prf_Log.txt") (case aum of {Nothing -> "Nobody"; Just chr -> snd (pID chr)} ++ " can wield the Aum staff.\n")


find_w_error :: (a -> Bool) -> [a] -> String -> a
find_w_error pred list message = fromMaybe (error message) (find pred list)

findIndex_w_error :: (a -> Bool) -> [a] -> String -> Int
findIndex_w_error pred list message = fromMaybe (error message) (findIndex pred list)