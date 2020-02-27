{-# LANGUAGE NamedFieldPuns #-}
module UpdateRawData (
  updateRawDataDB,
  updateRawDataDisp
) where

import Types
import Data.Word
import GeneralUtility

updateRawDataDB :: DatabaseStruct -> DatabaseStruct
updateRawDataDB db@DatabaseStruct{characters=chrs, classes=clss, items=items} =
  db{characters= map updateRawChr chrs, classes= map updateRawCls clss, items= map updateRawItem items}

updateRawDataDisp :: [DisposFile] -> [DisposFile]
updateRawDataDisp = map updateDisposFile

--- Utility ---
maskTupleize :: [Word8] -> Bool -> [(Word8, Bool)]
maskTupleize dat mask = map (\x -> (x, mask)) dat

maskTupleizeTupleList :: [(Word8, Word8)] -> Bool -> [(Word8, Bool)]
maskTupleizeTupleList dat mask = concatMap (\(x, y) -> (x, mask):[(y, mask)]) dat

--- Character ---
updateRawChr :: Character -> Character
updateRawChr chr@Character{rawData_chr=rawData} =
  let updateMasks = chrEntryMask chr
      newRawData = zipWithMaskTup rawData updateMasks
  in chr{rawData_chr=newRawData}

chrEntryMask :: Character -> [(Word8, Bool)]
chrEntryMask chr@Character{pID=pID, fID=fID, chrBases=chrBases, chrGrowths=chrGrowths, wRanks=wRanks, prf=prf} =
  maskTupleize (fst pID) True
  ++ maskTupleize (fst fID) True
  ++ replicate 4 (0, False)
  ++ maskTupleize chrBases True
  ++ maskTupleize chrGrowths True
  ++ maskTupleize wRanks True
  ++ replicate 2 (0, False)
  ++ (fst prf, True) : (snd prf, True)
   : replicate 0x2A (0, False)


--- Class ---
updateRawCls :: Class -> Class
updateRawCls cls@Class{rawData_cls=rawData} =
  let updateMasks = clsEntryMask cls
      newRawData = zipWithMaskTup rawData updateMasks
  in cls{rawData_cls=newRawData}

clsEntryMask :: Class -> [(Word8, Bool)]
clsEntryMask chr@Class{jID=jID, classBases=classBases,classGrowths=classGrowths, enemyGrowths=enemyGrowths,
 statCaps=statCaps, movement=(movetype, mov), reclassSet=reclassSet, promo=promo, wTypes=wTypes} =
  maskTupleize (fst jID) True
  ++ replicate 4 (0, False)
  ++ maskTupleize classBases True
  ++ maskTupleize classGrowths True
  ++ maskTupleize enemyGrowths True
  ++ maskTupleize statCaps True
  ++ [(movetype, True)] ++ [(mov, True)]
  ++ replicate 2 (0, False)
  ++ maskTupleize wTypes True
  ++ replicate 2 (0, False)
  ++ [(reclassSet, True)]
  ++ replicate 39 (0, False)


--- Item ---
updateRawItem :: Item -> Item
updateRawItem item@Weapon{rawData_item=rawData} =
  let updateMasks = weaponEntryMask item
      newRawData = zipWithMaskTup rawData updateMasks
  in item{rawData_item=newRawData}
updateRawItem item@Thing{rawData_item=rawData} =
  let updateMasks = thingEntryMask item
      newRawData = zipWithMaskTup rawData updateMasks
  in item{rawData_item=newRawData}

weaponEntryMask :: Item -> [(Word8, Bool)]
weaponEntryMask Weapon{iID=iID, itemNr=itemNr, namePtr, descPtr, iconNr, price, itemType=itemType, useEffect=useEffect,
 uses=uses, wpnLv=wpnLv, might, hit, crit, weight, range} =
     [(itemNr, True)]
  ++ replicate 3 (0, False)
  ++ maskTupleize (fst iID) True
  ++ maskTupleize (fst namePtr) True
  ++ maskTupleize (fst descPtr) True
  ++ [(iconNr, True)]
  ++ [(0, False)]
  ++ [(fst price, True), (snd price, True)]
  ++ [(itemType, True)]
  ++ [(useEffect, True)]
  ++ [(wpnLv, True)]
  ++ [(0, False)] -- Weapon Exp
  ++ [(uses, True)]
  ++ [(might, True), (hit, True), (crit, True), (weight, True), (fst range, True) , (snd range, True)]
  ++ replicate 0x1D (0, False)

thingEntryMask :: Item -> [(Word8, Bool)]
thingEntryMask Thing{iID=iID, itemNr=itemNr, namePtr, descPtr, iconNr, price, itemType=itemType, useEffect=useEffect, uses=uses} =
     [(itemNr, True)]
  ++ replicate 3 (0, False)
  ++ maskTupleize (fst iID) True
  ++ maskTupleize (fst namePtr) True
  ++ maskTupleize (fst descPtr) True
  ++ [(iconNr, True)]
  ++ [(0, False)]
  ++ [(fst price, True), (snd price, True)]
  ++ [(itemType, True)]
  ++ [(useEffect, True)]
  ++ [(0, False)]
  ++ [(0, False)] -- Weapon Exp
  ++ [(uses, True)]
  ++ replicate 6 (0, False)
  ++ replicate 0x1D (0, False)


--- Dispos ---
updateDisposFile :: DisposFile -> DisposFile
updateDisposFile dispFile@DisposFile{entries=entries}= dispFile{entries=map updateRawDispos entries}

updateRawDispos :: DisposEntry -> DisposEntry
updateRawDispos disp@DisposEntry{rawData_disp=rawData} =
  let updateMasks = disposEntryMask disp
      newRawData = zipWithMaskTup_errorOnMismatchedInput rawData updateMasks
  in disp{rawData_disp=newRawData}

disposEntryMask :: DisposEntry -> [(Word8, Bool)]
disposEntryMask DisposEntry{chrID=chrID, classID=classID, loadCrds=loadCrds, endCrds=endCrds, level=level, inventory=inventory} =
     (fst chrID, True): (snd chrID, True): (classID, True): (0, False): (fst loadCrds, True): (snd loadCrds, True)
   : (fst endCrds, True): (snd endCrds, True): (0, False): (fst level, True): [(snd level, True)]
   ++ replicate 5 (0, False)
   ++ maskTupleizeTupleList inventory True
   ++ replicate 0x38 (0, False)


--{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Use tuple-section" #-}