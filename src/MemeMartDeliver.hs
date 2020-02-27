{-# LANGUAGE NamedFieldPuns #-}
module MemeMartDeliver where

import Types
import Data.Word
import qualified Data.List as L
import qualified Data.Maybe as M












memeMartDeliverDB :: Order -> DatabaseStruct -> DatabaseStruct
memeMartDeliverDB order db@DatabaseStruct{items} = db{items=foldl deliverItemOrder items order}

deliverClassOrder :: [Class] -> (String, String) -> [Class]
deliverClassOrder classes ("option", "") = classes
deliverClassOrder classes ("option2", "") = classes
deliverClassOrder classes o = classes

deliverItemOrder :: [Item] -> (String, String) -> [Item]
deliverItemOrder items ("69", "") = map weapon69 items
deliverItemOrder items ("allitem.", nr) = map (\item -> allitem (M.fromMaybe item (L.find (\i -> (read nr :: Word8) == itemNr i) items)) item) items
deliverItemOrder items o = items

weapon69 :: Item -> Item
weapon69 i@Weapon{hit} = i{hit=69}
weapon69 i@Thing{} = i

allitem :: Item -> Item -> Item
allitem target cur = cur{namePtr=namePtr target, descPtr=descPtr target, iconNr=iconNr target}