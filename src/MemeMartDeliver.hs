{-# LANGUAGE NamedFieldPuns #-}
module MemeMartDeliver (
  memeMartDeliverDB,
  memeMartDeliverDispos
) where

import Types
import Data.Word
import qualified Data.List as L
import qualified Data.Maybe as M
import GeneralUtility
import qualified Data.Bits as Bit
import System.Random


memeMartDeliverDB :: StdGen -> Order -> DatabaseStruct -> DatabaseStruct
memeMartDeliverDB rng order db@DatabaseStruct{characters, items} = db{characters= foldl (deliverChrOrder rng) characters order, items=foldl deliverItemOrder items order}

memeMartDeliverDispos :: DatabaseStruct -> StdGen -> Order -> [DisposFile] -> [DisposFile]
memeMartDeliverDispos db rng order dispFs = let infinfrngs = map infiniteGenerators (infiniteGenerators rng)
  in foldl (deliverDisposOrder infinfrngs db) dispFs order


deliverDisposOrder :: [[StdGen]] -> DatabaseStruct -> [DisposFile] -> (String, String) -> [DisposFile]
deliverDisposOrder infinfrngs db disp ("dragon","me") = mapDisposUpdateRNG (dragonMe db) infinfrngs disp
deliverDisposOrder infinfrngs db disp ("dragon","all") = mapDisposUpdateRNG (dragonAll db) infinfrngs disp
deliverDisposOrder _ _ disp _ = disp

deliverChrOrder :: StdGen -> [Character] -> (String, String) -> [Character]
deliverChrOrder rng characters ("dragon", "me") = zipWith rngDragonPrf (map fst (iterate (next . snd) (next rng))) (filter (\x -> chrNr x < 0x3B) characters) ++ filter (\x -> chrNr x > 0x3A) characters
deliverChrOrder rng characters ("dragon", "all") = zipWith rngDragonPrf (map fst (iterate (next . snd) (next rng))) characters
deliverChrOrder rng characters o = characters

deliverItemOrder :: [Item] -> (String, String) -> [Item]
deliverItemOrder items ("69", "") = map weapon69 items
deliverItemOrder items ("allitem.", nr) = map (\item -> allitem (M.fromMaybe item (L.find (\i -> (read nr :: Word8) == itemNr i) items)) item) items
deliverItemOrder items ("devil", "") = map weaponDevil items
deliverItemOrder items ("brave", "") = map weaponBrave items
deliverItemOrder items o = items

weapon69 :: Item -> Item
weapon69 i@Weapon{hit} = i{hit=69}
weapon69 i@Thing{} = i

weaponDevil :: Item -> Item
weaponDevil i@Weapon{effectFlags} = let flag9 = (effectFlags !! 9)
  in i{effectFlags=replaceAtIndex effectFlags (flag9 Bit..|. 0x40) 9}
weaponDevil i@Thing{} = i

weaponBrave :: Item -> Item
weaponBrave i@Weapon{effectFlags} = let flag9 = (effectFlags !! 9)
  in i{effectFlags=replaceAtIndex effectFlags (flag9 Bit..|. 0x02) 9}
weaponBrave i@Thing{} = i

allitem :: Item -> Item -> Item
allitem target cur = cur{namePtr=namePtr target, descPtr=descPtr target, iconNr=iconNr target}

dragonMe :: DatabaseStruct -> StdGen -> DisposEntry -> DisposEntry
dragonMe db rng disp@DisposEntry{chrID}
  | snd chrID == 0 && fst chrID < 0x3B = dragonAll db rng disp
  | otherwise = disp

dragonAll :: DatabaseStruct -> StdGen -> DisposEntry -> DisposEntry
dragonAll DatabaseStruct{characters, items} rng disp@DisposEntry{chrID, classID, inventory} = let ran = randomR (0, 1) rng
  in disp{classID= 0x27 + fst ran, inventory= dragonInv (snd ran) (find_w_error (\x -> (fromIntegral (fst chrID) + (fromIntegral(snd chrID) :: Int) * 256) == chrNr x) characters "dragons chr") items inventory}

dragonInv :: StdGen -> Character -> [Item] -> [(Word8, Word8)] -> [(Word8, Word8)]
dragonInv rng Character{prf} items inv =
  let ran1 = randomR (0, 2) rng
      ran2 = randomR (0, 2) (snd ran1)
      ran3 = randomR (0, 2) (snd ran2)
      ran4 = randomR (0, 2) (snd ran3)
      inv1 = inv !! 0
      inv2 = inv !! 1
      inv3 = inv !! 2
      inv4 = inv !! 3
  in dragonInv2 (fst ran1) prf (L.find (\x -> itemNr x == fst inv1 - 1) items) (snd inv1)
   : dragonInv2 (fst ran2) prf (L.find (\x -> itemNr x == fst inv2 - 1) items) (snd inv2)
   : dragonInv2 (fst ran3) prf (L.find (\x -> itemNr x == fst inv3 - 1) items) (snd inv3)
   : [dragonInv2 (fst ran4) prf (L.find (\x -> itemNr x == fst inv4 - 1) items) (snd inv4)]

dragonInv2 :: Int -> (Word8, Word8) -> Maybe Item -> Word8 -> (Word8, Word8)
dragonInv2 _ _ Nothing _ = (0, 0)
dragonInv2 rn prf (Just i@Weapon{}) drop = (dragonstone rn prf, drop)
dragonInv2 _ _ (Just i@Thing{itemNr}) drop = (itemNr + 1, drop)

dragonstone :: Int -> (Word8, Word8) -> Word8
dragonstone 0 _ = 0x30
dragonstone 1 _ = 0x32
dragonstone 2 (0x10, 0) = 0x31
dragonstone 2 (0x01, 0x02) = 0x33
dragonstone _ _ = error "dragonstone"

rngDragonPrf :: Int -> Character -> Character
rngDragonPrf 0 chr = chr{prf=(0x01, 0x02)}
rngDragonPrf 1 chr = chr{prf=(0x10, 0)}