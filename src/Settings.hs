module Settings (
  modeSelectInit
) where

import System.IO
import Data.Word
import qualified Data.Char as Char
import MemeMart
import GeneralUtility
import Types
import DatabaseParse

emptySettings = Settings{reclass_set=NormalReclass, feclass_set=(False, False), bases_set=(False, False), growths_set=(False, False), items_set=(False, False)}


modeSelectInit :: DatabaseStruct -> IO (Settings, Order)
modeSelectInit db = do
  putStrLn "\nIf you want to configure Randomizer settings, press 1."
  putStrLn "If you want to visit Meme Mart, press 2."
  hFlush stdout

  let sel = (emptySettings, [])
  input <- awaitChar ['1', '2']
  newsel <- modeSelectParse db input sel
  modeSelect db newsel

modeSelect :: DatabaseStruct -> (Settings, Order) -> IO (Settings, Order)
modeSelect db (settings, order) = do
  putStrLn ("If you want to " ++ (if settings == emptySettings then "configure" else "redo your") ++ " Randomizer settings, press 1.")
  putStrLn ("If you want to " ++ (if null order then "visit" else "change your order at") ++ " Meme Mart, press 2.")
  putStrLn ("If you want to " ++ (if settings == emptySettings && null order then "exit without changes" else "apply your chosen options") ++ ", press 3.")
  hFlush stdout

  input <- awaitChar ['3', '4', '2', '1']
  -- print settings and order?
  if input == '3'
  then return (settings, order)
  else do newsel <- modeSelectParse db input (settings, order)
          modeSelect db newsel

modeSelectParse :: DatabaseStruct -> Char -> (Settings, Order) -> IO (Settings, Order)
modeSelectParse _ '1' (_, o) = ioTupleS gatherSettings o
modeSelectParse db '2' (s, _) = ioTupleO s (memeMart db)


gatherSettings :: IO Settings
gatherSettings = do

  putStrLn "\nSome initial notes:"
  putStrLn "* The randomization treats Lord and Thief unpromoted classes, while Manakete, Ballistician, and Freelancer are treated as promoted."
  putStrLn "* ^ For reclassing they are still treated as special classes."
  putStrLn "* Falcon Knight is added to the standard class pool regardless of settings."
  putStrLn "* Any weapons that get randomized become weapons of the same weapon rank. Items never get randomized."
  putStrLn "* The randomizer should never produce PRF weapons, Fake Falchion, or Real Imhullu."
  putStrLn "* Ymir continues to evade my attempts at finding where he gets loaded, so i can't log his class or inventory. They can be randomized, i just can't tell you to what."
  putStrLn "Press Enter to begin."
  hFlush stdout
  getLine

  putStrLn "\nWhat to do about reclassing? Press:"
  putStrLn "1 for no reclassing. The reccomended option, as the game otherwise won't be much different from normal."
  putStrLn "2 for randomized class sets. 3 class sets as ususal, but which class is in which one is random."
  putStrLn "3 for normal class sets."
--  putStrLn "4 for unified standard class sets. All standard classes can reclass to each other."
--  putStrLn "5 for unified class sets + Special class set. The same as 4, except now the special classes can also reclass to each other."
--  putStrLn "\nNotes:"
--  putStrLn "* Because the special classes can level to 30, they won't be put in class sets with standard classes."
  putStrLn "* The random class sets for before/after promotion don't line up."
  hFlush stdout
  reclassSetting <- reclassPrompt

  feclassSetting <- ynPrompt "Randomize classes? (y/n)\nNotes:\n* This option also randomizes who can use each PRF weapon to someone who can use it in their new class.\n* This option sets all characters' weapon ranks to equal their highest one.\n* Picking this option also randomizes inventores, but if you don't, randomizing inventories is still available as a standalone option."
  feclassSetting_e <- ynPrompt "Randomize enemy classes? (y/n)"
  itemSetting <- if feclassSetting then return False else ynPrompt "Randomize inventories? (y/n)\n* Weapons are replaced with alternatives of the same weapon rank. Items remain unchanged."
  itemSetting_e <- if feclassSetting_e then return False else ynPrompt "Randomize enemy inventories? (y/n)"
  basesSetting <- ynPrompt "Randomize personal bases? (y/n)"
  basesSetting_e <- ynPrompt "Randomize enemy personal bases? (y/n)\n* Note: Mostly affects bosses."
  growthsSetting <- ynPrompt "Randomize personal growths? (y/n)"
  growthsSetting_e <- ynPrompt "Randomize enemy personal growths? (y/n)\n* Note: Does very little."
  hFlush stdout

  return Settings{reclass_set=reclassSetting, feclass_set=(feclassSetting, feclassSetting_e), bases_set=(basesSetting, basesSetting_e), growths_set=(growthsSetting, growthsSetting_e), items_set=(itemSetting, itemSetting_e)}

reclassPrompt :: IO Reclass
reclassPrompt = do
  putStrLn "Waiting for input..."
  hFlush stdout
  reclassInput <- getLine
  reclassParse reclassInput

reclassParse :: String -> IO Reclass
reclassParse ('1': _) = return NoReclass
reclassParse ('2': _) = return RandomReclassSets
reclassParse ('3': _) = return NormalReclass
reclassParse ('4': _) = return UnifiedReclass
reclassParse ('5': _) = return CrazyReclass
reclassParse x = reclassPrompt




