module UnsafeTestkit where

import System.IO.Unsafe
import System.IO
import Numeric (showHex, showIntAtBase)

unsafePrint :: Show a => a -> a
unsafePrint x = unsafePerformIO $ do
  print x
  return x

unsafePrintLength :: [a] -> [a]
unsafePrintLength x = unsafePerformIO $ do
  putStrLn ("Length: " ++ show (length x))
  hFlush stdout
  return x


unsafePrintToLog :: (Show a, Integral a) => a -> a
unsafePrintToLog a = unsafePerformIO (do
  appendFile "C:\\Users\\jespo\\Documents\\FE Randomizers\\FE11\\unsafe_error_log.txt" (showHex a "" ++ "\n")
  return a)