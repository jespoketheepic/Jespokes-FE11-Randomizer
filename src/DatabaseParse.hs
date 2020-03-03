module DatabaseParse (
  parseDatabase,
  dbCharacters,
  dbClasses,
  dbItems,
  dbItemByItemNr,
  dbIIDNameByItemNr
) where

import qualified Data.ByteString as BS
import Data.Word
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GeneralUtility
import Types
import FixedEdits
import Table

--- Constants moved to: ---
import DatabaseConstants


parseDatabase :: BS.ByteString -> DatabaseStruct
parseDatabase file = DatabaseStruct {characters=listCharacters file, classes=listClasses file, items=listItems file}

--- Getters ---
dbCharacters :: DatabaseStruct -> [Character]
dbCharacters = characters

dbClasses :: DatabaseStruct -> [Class]
dbClasses = classes

dbItems :: DatabaseStruct -> [Item]
dbItems = items

dbItemByItemNr :: DatabaseStruct -> Word8 -> Item
dbItemByItemNr db i = head (filter (\x -> i == itemNr x) (items db))

dbIIDNameByItemNr :: DatabaseStruct -> Word8 -> String
dbIIDNameByItemNr db nr = snd (iID (dbItemByItemNr db nr))


--- Utility ---
foldPointerIntoOffset :: ([Word8] -> Int)
foldPointerIntoOffset = foldr (\x y -> fromEnum x + y *256) 0

getStats :: Offset -> BS.ByteString -> [Word8]
getStats = getStuff 8

getPointer :: Offset -> BS.ByteString -> [Word8]
getPointer = getStuff 4

getStuff :: Int -> Offset -> BS.ByteString -> [Word8]
getStuff i o xs = BS.unpack $ BS.take i (BS.drop o xs)

readID :: BS.ByteString -> String
readID xs = T.unpack $ TE.decodeLatin1 (BS.takeWhile (/=0) xs)

getID :: Offset -> BS.ByteString -> BS.ByteString -> ID
getID entryOffset xs ogxs = (getPointer entryOffset xs, getIDname (getPointer entryOffset xs) ogxs)

--- Give a file cropped to the pointer reference, not individual entry!
getIDname :: [Word8] -> BS.ByteString -> String
getIDname pointer ogxs = if pointer == [0,0,0,0]
then "(none)"
else readID (BS.drop (foldPointerIntoOffset pointer) ogxs)


-- Characters --
listCharacters :: BS.ByteString -> [Character]
listCharacters xs = listCharactersR (BS.drop headerSize xs) (BS.drop headerSize xs) 0

listCharactersR :: BS.ByteString -> BS.ByteString -> Int -> [Character]
listCharactersR xs ogxs i
  | i == chrCount = []
  | otherwise = makeCharacter i xs ogxs : listCharactersR (BS.drop chrEntrySize xs) ogxs (i+1)

makeCharacter :: Int -> BS.ByteString -> BS.ByteString -> Character
makeCharacter nr xs ogxs = Character{chrNr=nr, pID=getPID xs ogxs, fID=getFID xs ogxs, chrBases=getChrBases xs, chrGrowths=getChrGrowths xs, wRanks=getWRanks xs, chrClass=Nothing, prf=getPrf xs, rawData_chr=(bytestringGetN chrEntrySize xs)}

getPID :: BS.ByteString -> BS.ByteString -> ID
getPID = getID 0

getFID :: BS.ByteString -> BS.ByteString -> ID
getFID xs ogxs = getID 4 xs ogxs

getChrBases :: BS.ByteString -> [Word8]
getChrBases = getStats 12

getChrGrowths :: BS.ByteString -> [Word8]
getChrGrowths = getStats 20

getWRanks :: BS.ByteString -> [Word8]
getWRanks = getStuff 6 0x1C

getPrf :: BS.ByteString -> (Word8, Word8)
getPrf xs = (BS.index xs 0x24, BS.index xs 0x25)

-- Classes --
listClasses :: BS.ByteString -> [Class]
listClasses xs = listClassesR (BS.drop classTableOffset xs) (BS.drop headerSize xs) 0

listClassesR :: BS.ByteString -> BS.ByteString -> Int -> [Class]
listClassesR xs ogxs i
  | i == classCount = []
  | otherwise = makeClass xs ogxs i : listClassesR (BS.drop classEntrySize xs) ogxs (i+1)

makeClass :: BS.ByteString -> BS.ByteString -> Int -> Class
makeClass xs ogxs nr = Class{classNr=fromIntegral nr, promotesFrom=Nothing, valid=markInvalidClass nr, jID=(getJID xs ogxs), classBases=(getClassBases xs), classGrowths=(getClassGrowths xs)
  , enemyGrowths=(getEnemyGrowths xs), statCaps=(getStatCaps xs), movement=(getMovement xs), wTypes=(getWTypes xs), reclassSet=(getReclassSet xs)
  , promo=(getPromo xs nr), genericFace=getGenericFace xs, rawData_cls=(bytestringGetN classEntrySize xs)}

getJID :: BS.ByteString -> BS.ByteString -> ID
getJID = getID 0x00

getClassBases :: BS.ByteString -> [Word8]
getClassBases = getStats 8

getClassGrowths :: BS.ByteString -> [Word8]
getClassGrowths = getStats 16

getEnemyGrowths :: BS.ByteString -> [Word8]
getEnemyGrowths = getStats 24

getStatCaps :: BS.ByteString -> [Word8]
getStatCaps = getStats 32

getMovement :: BS.ByteString -> (Word8, Word8)
getMovement xs = (BS.index xs 40, BS.index xs 41)

getWTypes :: BS.ByteString -> [Word8]
getWTypes = getStuff 6 0x2C

getReclassSet :: BS.ByteString -> Word8
getReclassSet xs = BS.index xs reclassSetOffset

getPromo :: BS.ByteString -> Int -> Promo
getPromo xs = getPromo2 (getReclassSet xs)

getPromo2 :: Word8 -> Int -> Promo
getPromo2 x nr
  | x == 0 = specialPromo nr
  | x < 4 = Base
  | otherwise = Promoted

specialPromo :: Int -> Promo
specialPromo 0x00 = Base
specialPromo 0x26 = Base
specialPromo 0x2B = Base
specialPromo x = Promoted

getGenericFace :: BS.ByteString -> Bool
getGenericFace xs = foldl (\x y -> x + (fromIntegral y)) (0 :: Int) (getPointer 0x48 xs) == 0
--if (foldl (\x y -> (fromIntegral x :: Int) + (fromIntegral y :: Int)) 0 (getPointer 0x48 xs)) == 0 then [0x03, 0xDE, 0x00, 0x00, 0xA7, 0xBE, 0x00, 0x00] else getStats 0x44 xs

-- Items --
listItems :: BS.ByteString -> [Item]
listItems xs = listItemsR (BS.drop itemTableOffset xs) (BS.drop headerSize xs) 0

listItemsR :: BS.ByteString -> BS.ByteString -> Int -> [Item]
listItemsR xs ogxs i
  | i == itemCount = []
  | otherwise = makeItem xs ogxs : listItemsR (BS.drop itemEntrySize xs) ogxs (i+1)

makeItem :: BS.ByteString -> BS.ByteString -> Item
makeItem xs = makeItemSplit (BS.index xs 0x14) xs

makeItemSplit :: Word8 -> BS.ByteString -> BS.ByteString -> Item
makeItemSplit 8 xs ogxs = Thing{itemNr=(BS.index xs 0), randomGive=markDontGive (BS.index xs 0), iID=(getID 0x04 xs ogxs)
      , namePtr=(getID 0x08 xs ogxs), descPtr=(getID 0x0C xs ogxs), iconNr=(BS.index xs 0x10), price=(BS.index xs 0x12, BS.index xs 0x13)
      , itemType=(BS.index xs 0x14), useEffect=(BS.index xs 0x15), uses=(BS.index xs 0x18), effectFlags=(getEffectFlags xs), rawData_item=(bytestringGetN itemEntrySize xs)}
makeItemSplit i xs ogxs = Weapon{itemNr=(BS.index xs 0), randomGive= markDontGive (BS.index xs 0), iID=(getID 0x04 xs ogxs)
      , namePtr=(getID 0x08 xs ogxs), descPtr=(getID 0x0C xs ogxs), iconNr=(BS.index xs 0x10), price=(BS.index xs 0x12, BS.index xs 0x13)
      , itemType=(BS.index xs 0x14), useEffect=(BS.index xs 0x15), uses=(BS.index xs 0x18), wpnLv=(BS.index xs 0x16), might=(BS.index xs 0x19)
      , hit=(BS.index xs 0x1A), crit=(BS.index xs 0x1B), weight=(BS.index xs 0x1C), range=(BS.index xs 0x1D, BS.index xs 0x1E)
      , effectFlags=(getEffectFlags xs), rawData_item=(bytestringGetN itemEntrySize xs)}


getEffectFlags :: BS.ByteString -> [Word8]
getEffectFlags = getStuff 0x1D 0x1F





{-# ANN module "HLint: ignore Redundant bracket" #-}