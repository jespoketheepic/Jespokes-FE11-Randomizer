module Main where

import Lib
import System.IO
import GeneralUtility

main :: IO ()
main = do
  -- Intro
  putStrLn "Welcome to Jespoke's FE11 randomizer.\nPlease make sure \"Jespoke's FE11 Randomizer\", \"CompressLZ77.exe\", and the folder \"DSLazy\" with an are all in the same folder together."
  putStrLn "First use DSLazy to unpack your Fire Emblem: Shadow Dragon ROM. After you are done with the randomizer, use DSLazy to pack the randomized files into a new ROM."
  hFlush stdout
  continue <- oneTwoPrompt "Press 1 to start, or 2 to abort."
  if continue then action else return ()