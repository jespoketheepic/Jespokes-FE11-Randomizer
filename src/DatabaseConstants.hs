module DatabaseConstants where

chrEntrySize = 0x50
headerSize = 0x20
chrCount = 0x176
classEntrySize = 0x5C
classCount = 0x30
classTableOffset = 0x7500
reclassSetOffset = 0x34 :: Int
itemCount = 0x66
itemEntrySize = 0x3C
itemTableOffset = 0x878C
disposEntrySize = 0x50 :: Int

--- Derived constants
chr_TableSize = chrCount * chrEntrySize :: Int
chr_TableEnd = headerSize + chr_TableSize :: Int
chr_class_TableGap = classTableOffset - chr_TableEnd :: Int
class_TableSize = classEntrySize * classCount
class_TableEnd = classTableOffset + class_TableSize
class_item_TableGap = itemTableOffset - class_TableEnd
item_TableSize = itemEntrySize * itemCount
item_TableEnd = itemTableOffset + item_TableSize


{-# ANN module "HLint: ignore Use camelCase" #-}