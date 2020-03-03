{-# LANGUAGE NamedFieldPuns #-}
module Randomize (
  randomize
) where

import GeneralUtility
import Types
import System.Random
import Data.Word
import qualified Data.List as L
import qualified Data.Maybe as M
import FixedEdits

import UnsafeTestkit

randomize :: DatabaseStruct -> [DisposFile] -> StdGen -> Settings -> (DatabaseStruct, [DisposFile])
randomize db disposss randG Settings{reclass_set=re, feclass_set=cl, bases_set=b, growths_set=g, items_set=it} =
  let infiniteRNGs1 = infiniteGenerators randG
      infiniteRNGs2 = infiniteGenerators (head infiniteRNGs1)
      infiniteRNGs3 = infiniteGenerators (head infiniteRNGs2)
      infiniteRNGs4 = infiniteGenerators (head infiniteRNGs3)
      infiniteRNGs5 = infiniteGenerators (head infiniteRNGs4)

      db2 = randBases b db infiniteRNGs1
      db3 = randGrowths g db2 infiniteRNGs2
      db4 = randReclass re db3 infiniteRNGs3
      -- These two actually doing anything is mutually exclusive, so using the same rngs is fine.
      disposss2 = randClasses cl disposss (classes db4) (items db4) (map infiniteGenerators infiniteRNGs4)
      disposss3 = randItemsYN it disposss2 (classes db4) (items db4) (map infiniteGenerators infiniteRNGs4)
      db5 = boostWeaponRanks cl db4
       -- Redo noteChrClass to note down the potentially newly changed classes
      db6 = noteChrClass db5 disposss3
      db7 = prfDistribution (fst cl) infiniteRNGs5 db6
  in  (db7, disposss3)


--- Bases and Growths ---
randBases :: (Bool, Bool) -> DatabaseStruct -> [StdGen] -> DatabaseStruct
randBases (False, False) db randGs = db
randBases who db randGs = randBasesGo who db randGs

randBasesGo :: (Bool, Bool) -> DatabaseStruct -> [StdGen] -> DatabaseStruct
randBasesGo (p, e) db@DatabaseStruct{characters=chrs} randGs = db{characters=map (\(chr, randG) -> chr{chrBases= if (chrNr chr < 0x3B && p) || (chrNr chr > 0x3A && e) then shuffleBases randG (chrBases chr) else chrBases chr}) (zip chrs randGs)}


randGrowths :: (Bool, Bool) -> DatabaseStruct -> [StdGen] -> DatabaseStruct
randGrowths (False, False) db randGs = db
randGrowths who db randGs = randGrowthsGo who db randGs

randGrowthsGo :: (Bool, Bool) -> DatabaseStruct -> [StdGen] -> DatabaseStruct
randGrowthsGo (p, e) db@DatabaseStruct{characters=chrs} randGs = db{characters=map (\(chr, randG) -> chr{chrGrowths= if (chrNr chr < 0x3B && p) || (chrNr chr > 0x3A && e) then shuffleGrowths randG (chrGrowths chr) else chrGrowths chr}) (zip chrs randGs)}


--- Class ----
randClasses :: (Bool, Bool) -> [DisposFile] -> [Class] -> [Item] -> [[StdGen]] -> [DisposFile]
randClasses (False, False) disposss classes items infiniteInfiniteRNGs = disposss
randClasses who disposss classes items infiniteInfiniteRNGs = map (randClassesGo who classes items infiniteInfiniteRNGs) disposss

randClassesGo :: (Bool, Bool) -> [Class] -> [Item] -> [[StdGen]] -> DisposFile -> DisposFile
randClassesGo who classes items infiniteInfiniteRNGs disp@DisposFile{entries=entries} = disp{entries=map (\(entry, rngs) -> randClassesFriendFoe who (chrID entry) classes items rngs entry) (zip entries infiniteInfiniteRNGs)}

randClassesFriendFoe :: (Bool, Bool) -> (Word8, Word8) -> [Class] -> [Item] -> [StdGen] -> DisposEntry -> DisposEntry
randClassesFriendFoe (p, e) (chrID1, chrID2) classes items rngs entry
  | chrID2 == 0 && chrID1 < 0x3B && p = randClass classes classes items rngs entry
  | (chrID2 > 0 || chrID1 > 0x3A) && e = randClass (filter (\x -> not (genericFace x)) classes) classes items rngs entry
  | otherwise = entry

randClass :: [Class] -> [Class] -> [Item] -> [StdGen] -> DisposEntry -> DisposEntry
randClass allowedClasses classes items rngs disp@DisposEntry{classID=classID, inventory=inventory} = disp{classID=randomClassOfSamePromo allowedClasses classes (head rngs) classID
  , inventory=matchWeaponsToClass inventory items (tail rngs) (getClassByClassNr classes (randomClassOfSamePromo allowedClasses classes (head rngs) classID))}

randomClassOfSamePromo :: [Class] -> [Class] -> StdGen -> Word8 -> Word8
randomClassOfSamePromo allowedClasses classes rng classID = let
  promoLv = promo (getClassByClassNr classes classID)
  clsOptions = filter (\cls -> valid cls && promoLv == promo cls) allowedClasses
  in classNr (randomListElement clsOptions rng)

getClassByClassNr :: [Class] -> Word8 -> Class
getClassByClassNr classes classID = find_w_error (\cls -> classID == classNr cls) classes "getClassByClassNr"

getItemByItemNr :: [Item] -> Word8 -> Item
getItemByItemNr items itemID = find_w_error (\item -> itemID == itemNr item) items "getItemByItemNr"

--- Items (Usually invoked by the randClass chain)
randItemsYN :: (Bool, Bool) -> [DisposFile] -> [Class] -> [Item] -> [[StdGen]] -> [DisposFile]
randItemsYN (False, False) disposss classes items infiniteInfiniteRNGs = disposss
randItemsYN who disposss classes items infiniteInfiniteRNGs = map (randItemsGo who classes items infiniteInfiniteRNGs) disposss

randItemsGo :: (Bool, Bool) -> [Class] -> [Item] -> [[StdGen]] -> DisposFile -> DisposFile
randItemsGo (p, e) classes items infiniteInfiniteRNGs disp@DisposFile{entries=entries} = disp{entries=map (\(entry, rngs) -> if (fst (chrID entry) < 0x3B && p) || (fst (chrID entry) > 0x3A && e) then randItems classes items rngs entry else entry) (zip entries infiniteInfiniteRNGs)}

randItems :: [Class] -> [Item] -> [StdGen] -> DisposEntry -> DisposEntry
randItems classes items rngs disp@DisposEntry{classID=classID, inventory=inventory} =
  disp{inventory=matchWeaponsToClass inventory items rngs (getClassByClassNr classes classID)}

matchWeaponsToClass :: [(Word8, Word8)] -> [Item] -> [StdGen] -> Class -> [(Word8, Word8)]
matchWeaponsToClass weapons items rngs cls = zipWith (\item rng -> matchWeaponToClass item items cls rng) weapons rngs

matchWeaponToClass :: (Word8, Word8) -> [Item] -> Class -> StdGen -> (Word8, Word8)
matchWeaponToClass (cur, drop) items cls rng =
  let currentItem = L.find (\item -> cur - 1 == itemNr item) items
  in case currentItem of {Nothing -> (cur, drop); Just something -> (itemNr (randomizeItem something items cls rng) + 1, drop)}

randomizeItem :: Item -> [Item] -> Class -> StdGen -> Item
randomizeItem curItem items cls@Class{wTypes=wTypes, classNr=classNr} rng
  | itemType curItem == 8 = curItem
  | itemType curItem == 7 || itemType curItem == 6 = randomizeItem (items !! 2) items cls rng -- Silver Sword is used as a random B-rank weapon to simulate all ballistae/dragonstones being B-rank
  | classNr == 0x27 || classNr == 0x28 = randomListElement (filter (\x -> itemType x == 6 && randomGive x) items) rng
  | classNr == 0x29 = head (filter (\x -> itemType x == 7) items)
  | sumToInt wTypes == 0 = curItem
  | otherwise = randomListElement (filter (\x -> randomGive x && itemType x < 6 && wTypes !! fromIntegral (itemType x) /= 0 && wpnLv x == wpnLv curItem) items) rng


--- Reclass ---
randReclass :: Reclass -> DatabaseStruct -> [StdGen] -> DatabaseStruct
randReclass reclass db@DatabaseStruct{classes} randGs = if reclass /= NormalReclass
then db{classes= (randReclassGo reclass (filter (valid) classes) randGs) ++ (filter (\x -> not (valid x)) classes)}
else db

randReclassGo :: Reclass -> [Class] -> [StdGen] -> [Class]
randReclassGo NoReclass classes _ = map (\cls -> cls{reclassSet=0}) classes
randReclassGo RandomReclassSets classes randGs = fst (foldl (\(list, setCounts) (cls, rng) -> randReclassSets rng cls (list, setCounts)) ([],[0,0,0,0,0,0,0]) (zip classes randGs))
randReclassGo UnifiedReclass classes _ = map unifyClassSets classes
randReclassGo CrazyReclass classes _ = map unifyClassSetsCrazy classes

randReclassSets :: StdGen -> Class -> ([Class], [Int]) -> ([Class], [Int])
randReclassSets randG cls@Class{reclassSet=reclass, promotesFrom} (list, setCounts)
  | reclass == 0 = (list ++ [cls], setCounts)
  | reclass < 4 = (list, setCounts)
  | reclass > 3 && M.isJust promotesFrom =
      let ran = randomR_Word8 (4, 6) randG
          randGNext = snd (next randG)
      in if setCounts !! fromIntegral ran >= 7
      then randReclassSets randGNext cls (list, setCounts)
      else (cls{reclassSet= ran} : (M.fromMaybe (error "How??") promotesFrom){reclassSet= ran-3} : list, incrementAtIndex setCounts (fromIntegral ran))
  | otherwise =
      let ran = randomR_Word8 (4, 6) randG
          randGNext = snd (next randG)
      in if setCounts !! fromIntegral ran >= 7
      then randReclassSets randGNext cls (list, setCounts)
      else (cls{reclassSet= ran} : list, incrementAtIndex setCounts (fromIntegral ran))

unifyClassSets :: Class -> Class
unifyClassSets cls@Class{reclassSet=reclass}
  | reclass == 0 = cls
  | reclass < 4 = cls{reclassSet=1}
  | otherwise = cls{reclassSet=4}

unifyClassSetsCrazy :: Class -> Class
unifyClassSetsCrazy cls@Class{reclassSet=reclass}
  | reclass == 0 = cls{reclassSet=2}
  | reclass < 4 = cls{reclassSet=1}
  | otherwise = cls{reclassSet=4}


--- Other ---
shuffleBases :: StdGen -> Stats -> Stats
shuffleBases randG stats =
  let statTotal = sumWord8ToInt_signed stats
      randNrsRaw = take 8 (randoms randG) :: [Double]
      randNrs = init randNrsRaw ++ [last randNrsRaw / 2]
      randTotal = sum randNrs
      truncStats = map (\randNr -> floor(fromIntegral statTotal * randNr / randTotal)) randNrs
      remainder = statTotal - sumToInt truncStats
  in  spreadRemainderBa truncStats (fromIntegral remainder)

shuffleGrowths :: StdGen -> Stats -> Stats
shuffleGrowths randG stats =
  let statTotal = sumWord8ToInt_signed stats
      randNrsRaw = take 8 (randoms randG) :: [Double]
      randNrs = init randNrsRaw ++ [last randNrsRaw / 2]
      randTotal = sum randNrs
      truncStats = map (\randNr -> if randGrowth randNr statTotal randTotal > 125 then 125 else fromIntegral (randGrowth randNr statTotal randTotal)) randNrs :: [Word8]
      remainder = statTotal - sumToInt truncStats
  in  spreadRemainderGr truncStats (fromIntegral remainder)

randGrowth :: Double -> Int -> Double -> Int
randGrowth randNr statTotal randTotal = 5 * floor(fromIntegral statTotal * randNr / (randTotal * 5))

spreadRemainderBa :: [Word8] -> Word8 -> [Word8]
spreadRemainderBa stats rem
  | rem == 0 = stats
  | (stats !! 5) + rem <= 125 = take 5 stats ++ ((stats !! 5) + rem):(drop 6 stats)
  | (stats !! 5) < 125 = spreadRemainderGr (take 5 stats ++ ((stats !! 5) + 1):(drop 6 stats)) (rem-1)
  | otherwise = (head stats + rem):(drop 1 stats)

spreadRemainderGr :: [Word8] -> Word8 -> [Word8]
spreadRemainderGr stats rem
  | rem == 0 = stats
  | head stats + rem <= 125 = (head stats + rem):(drop 1 stats)
  | head stats < 125 = spreadRemainderGr ((head stats + 1):(drop 1 stats)) (rem-1)
  | otherwise = take 5 stats ++ [(stats !! 5) + rem] ++ drop 6 stats

boostWeaponRanks :: (Bool, Bool) -> DatabaseStruct -> DatabaseStruct
boostWeaponRanks (False, False) db = db
boostWeaponRanks who db@DatabaseStruct{characters} = db{characters=map (boostWeaponRanksSingle who) characters}

boostWeaponRanksSingle :: (Bool, Bool) -> Character -> Character
boostWeaponRanksSingle (p, e) chr@Character{chrNr, wRanks} = chr{wRanks= if ((chrNr < 0x3B) && p) || ((chrNr > 0x3A) && e) then replicate 6 (if maximum wRanks == 0 then 0x87 else maximum wRanks) else wRanks}

--- PRF ---
prfDistribution :: Bool -> [StdGen] -> DatabaseStruct -> DatabaseStruct
prfDistribution False _ db = db
prfDistribution True rngs db@DatabaseStruct{characters} = db{characters=prfDistribution2 characters rngs}

prfDistribution2 :: [Character] -> [StdGen] -> [Character]
prfDistribution2 characters rngg = let
  swordCands = filter (\chr -> case chrClass chr of {Nothing -> False; Just cls -> chrNr chr < 0x34 && head (wTypes cls) /= 0}) characters
  lanceCands = filter (\chr -> case chrClass chr of {Nothing -> False; Just cls -> chrNr chr < 0x34 && (wTypes cls !! 1) /= 0}) characters
  tomeCands = filter (\chr -> case chrClass chr of {Nothing -> False; Just cls -> chrNr chr < 0x34 && (wTypes cls !! 4) /= 0}) characters
  staffCands = filter (\chr -> case chrClass chr of {Nothing -> False; Just cls -> chrNr chr < 0x34 && (wTypes cls !! 5) /= 0}) characters
  stoneCands = filter (\chr -> case chrClass chr of {Nothing -> False; Just cls -> chrNr chr < 0x34 && (classNr cls == 0x27 || classNr cls == 0x28)}) characters

  characters_prfWiped = map (\chr -> chr{prf=(0, 0)}) characters

  rapier = selectCand swordCands (head rngg) (0x04, 0)
  wingSpear = selectCand lanceCands (rngg !! 1) (0x29, 0)
  excalibur = selectCand tomeCands (rngg !! 2) (0x80, 0)
  aura = selectCand (filter (\x -> case excalibur of {Nothing -> False; Just chr -> pID chr /= pID x}) tomeCands) (rngg !! 3) (0x01, 0x01)
  hammerne = selectCand staffCands (rngg !! 4) (0x41, 0)
  aum = selectCand (filter (\x -> case hammerne of {Nothing -> False; Just chr -> pID chr /= pID x}) staffCands) (rngg !! 5) (0x09, 0)
  divine = selectCand stoneCands (rngg !! 6) (0x01, 0x02)
  earth = selectCand (filter (\x -> case divine of {Nothing -> False; Just chr -> pID chr /= pID x}) stoneCands) (rngg !! 7) (0x10, 0)
  prfHolders = foldl (\list maybeHolder -> case maybeHolder of {Nothing -> list; Just holder -> holder:list}) [] [rapier, wingSpear, excalibur, aura, divine, earth, hammerne, aum]
  in map (\chr -> M.fromMaybe chr (L.find (\prf -> pID chr == pID prf) prfHolders)) characters_prfWiped

selectCand :: [Character] -> StdGen -> (Word8, Word8) -> Maybe Character
selectCand [] _ _ = Nothing
selectCand candidates rng prfVal = Just (randomListElement candidates rng){prf=prfVal}




{-# ANN spreadRemainderGr "HLint: ignore Redundant bracket" #-}
{-# ANN spreadRemainderBa "HLint: ignore Redundant bracket" #-}