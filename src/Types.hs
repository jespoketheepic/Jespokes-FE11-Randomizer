module Types (
  DatabaseStruct(..),
  ID,
  Character(..),
  Class(..),
  Item(..),
  DisposEntry(..),
  Pointer,
  Stats,
  Directory,
  Order,
  Settings(..),
  Reclass(..),
  Promo(..),
  Offset,
  TableEntry(..),
  DisposFile(..)
) where

import Data.Word

class TableEntry a where
  rawData :: a -> [Word8]

data DatabaseStruct = DatabaseStruct{
  characters :: [Character],
  classes :: [Class],
  items :: [Item]
} deriving (Show)

data Character = Character{
  chrNr :: Int,
  pID :: ID,
  fID :: ID,
  chrBases :: Stats,
  chrGrowths :: Stats,
  wRanks :: [Word8],
  prf :: (Word8, Word8),
  chrClass :: Maybe Class,
  rawData_chr :: [Word8]
} deriving (Show)
instance TableEntry Character where
  rawData = rawData_chr

data Class = Class{
  classNr :: Word8,
  valid :: Bool,
  jID :: ID,
  classBases :: Stats,
  classGrowths :: Stats,
  enemyGrowths :: Stats,
  statCaps :: Stats,
  movement :: (Word8, Word8), --- (Movement Type, Move)
  wTypes :: [Word8],
  reclassSet :: Word8,
  promo :: Promo,
  rawData_cls :: [Word8]
} deriving (Show)
instance TableEntry Class where
  rawData = rawData_cls

data Item = Weapon{
    itemNr :: Word8,
    iID :: ID,
    namePtr :: ID,
    descPtr :: ID,
    iconNr :: Word8,
    price :: (Word8, Word8),
    itemType :: Word8,
    useEffect :: Word8,
    uses :: Word8,
    wpnLv :: Word8,
    might :: Word8,
    hit :: Word8,
    crit :: Word8,
    weight :: Word8,
    range :: (Word8, Word8),
    randomGive :: Bool,
    rawData_item :: [Word8]
  } | Thing{
    itemNr :: Word8,
    iID :: ID,
    namePtr :: ID,
    descPtr :: ID,
    iconNr :: Word8,
    price :: (Word8, Word8),
    itemType :: Word8,
    useEffect :: Word8,
    uses :: Word8,
    randomGive :: Bool,
    rawData_item :: [Word8]
  } deriving (Show)
instance TableEntry Item where
  rawData = rawData_item

data Settings = Settings {
  reclass_set :: Reclass,
  feclass_set :: (Bool, Bool),
  bases_set :: (Bool, Bool),
  growths_set :: (Bool, Bool),
  items_set :: (Bool, Bool)
} deriving (Show, Eq)

data DisposEntry = DisposEntry {
  chrID :: (Word8, Word8),
  classID :: Word8,
  loadCrds :: (Word8, Word8),
  endCrds :: (Word8, Word8),
  level :: (Word8, Word8), -- I there are two for auto-leveling? They are usually the same.
  -- there is an ability byte here that could be researched
  inventory :: [(Word8, Word8)], -- 4 of them, the second byte is a single flag for dropable
  rawData_disp :: [Word8]
} deriving (Show, Eq)
instance TableEntry DisposEntry where
  rawData = rawData_disp

data DisposFile = DisposFile {
  dispfileID :: String,
  filePath :: FilePath,
  entryStartOffset :: Int,
  entries :: [DisposEntry]
} deriving (Show, Eq)

data Promo = Base | Promoted deriving (Show, Eq)
data Reclass = NoReclass | RandomReclassSets | NormalReclass | UnifiedReclass | CrazyReclass deriving (Show, Eq, Ord)

--- 4 long by convention
type Pointer = [Word8]
--- 8 long by convention
type Stats = [Word8]
type Directory = String
type Order = [(String, String)]
type ID = (Pointer, String)
type Offset = Int
