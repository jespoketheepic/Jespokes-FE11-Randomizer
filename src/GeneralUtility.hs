{-# LANGUAGE NamedFieldPuns #-}
module GeneralUtility (
  getWorkingDirectory,
  ynPrompt,
  ynBoolParse,
  oneTwoPrompt,
  oneTwoBoolParse,
  awaitChar,
  ifThenElse,
  rangeValidation,
  isThisStringAnIntInRange,
  itemNrRange,
  splitAtFirst,
  ioTupleS,
  ioTupleO,
  randomR_Word8,
  bytestringGetN,
  zipWithMask_errorOnMismatchedInput,
  zipWithMask,
  zipWithMaskTup,
  zipWithMaskTup_errorOnMismatchedInput,
  infiniteGenerators,
  sumToInt,
  randomListElement,
  sumWord8ToInt_signed,
  word8ToInt_signed,
  replaceAtIndex
) where

import Data.Word
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified System.Directory as Dir
import System.IO
import Types
import qualified System.Random as R

import UnsafeTestkit

--- Constants ---
headerSize = 0x20
classTableOffset = 0x7500
itemTableOffset = 0x878C
itemNrRange = (0x00 :: Int, 0x65 :: Int)


--- Temporary measure
getWorkingDirectory :: IO String
getWorkingDirectory = Dir.getCurrentDirectory

ynPrompt :: String -> IO Bool
ynPrompt prompt = do
  putStrLn prompt
  hFlush stdout
  input <- getLine
  ynBoolParse input (ynPrompt prompt)

--- Takes a Char, and a function to call if that char is not 'y' or 'n'.
ynBoolParse :: String -> IO Bool -> IO Bool
ynBoolParse [] f = f
ynBoolParse ('y': _) _ = return True
ynBoolParse ('n': _) _ = return False
ynBoolParse x f = f

oneTwoPrompt :: String -> IO Bool
oneTwoPrompt prompt = do
  putStrLn prompt
  hFlush stdout
  input <- getLine
  oneTwoBoolParse input (oneTwoPrompt prompt)

--- Takes a Char, and a function to call if that char is not '1' or '2'.
oneTwoBoolParse :: String -> IO Bool -> IO Bool
oneTwoBoolParse [] f = f
oneTwoBoolParse ('1': _) _ = return True
oneTwoBoolParse ('2': _) _ = return False
oneTwoBoolParse x f = f

awaitChar :: [Char] -> IO Char
awaitChar chars = do
  input <- getLine
  awaitChar2 input chars

awaitChar2 :: String -> [Char] -> IO Char
awaitChar2 [] chars = awaitChar chars
awaitChar2 (x: _) chars = ifThenElse (x `elem` chars) (return x) (awaitChar chars)


ifThenElse :: Bool -> a -> a -> a
ifThenElse True x y = x
ifThenElse False x y = y

rangeValidation :: (Num a, Ord a) => a -> (a, a) -> Bool
rangeValidation value (low, high) = (low < value) && (value < high)

isThisStringAnIntInRange :: (Int, Int) -> String -> Bool
isThisStringAnIntInRange range string = all C.isDigit string && rangeValidation (read string) range

splitAtFirst :: Eq a => a -> [a] -> ([a],[a])
splitAtFirst splitter list = case L.elemIndex splitter list of
  Just i -> splitAt (i+1) list
  Nothing -> (list, [])

ioTupleS :: IO Settings -> Order -> IO (Settings, Order)
ioTupleS s o = do set <- s
                  return (set , o)

ioTupleO :: Settings -> IO Order -> IO (Settings, Order)
ioTupleO s o = do ord <- o
                  return (s, ord)

randomR_Word8 :: R.RandomGen g => (Word8, Word8) -> g -> Word8
randomR_Word8 (low, high) randG =
  let (nr, _) = R.randomR (low, high) randG
  in nr

bytestringGetN :: Int -> BS.ByteString -> [Word8]
bytestringGetN i bs = take i (BS.unpack bs)

infiniteGenerators :: (R.RandomGen g) => g -> [g]
infiniteGenerators = L.unfoldr (Just . R.split)


--- False keeps input 1, true keeps input 2
--- The intuition is "Should we update the old(1) to the new(2)?"
zipWithMask :: [a] -> [a] -> [Bool] -> [a]
zipWithMask [] _ _ = []
zipWithMask _ [] _ = []
zipWithMask _ _ [] = []
zipWithMask (x:xs) (y:ys) (False:masks) = x:(zipWithMask xs ys masks)
zipWithMask (x:xs) (y:ys) (True:masks) = y:(zipWithMask xs ys masks)

zipWithMaskTup :: [a] -> [(a, Bool)] -> [a]
zipWithMaskTup [] _ = []
zipWithMaskTup _ [] = []
zipWithMaskTup (x:xs) ((y, False):ys) = x:(zipWithMaskTup xs ys)
zipWithMaskTup (x:xs) ((y, True):ys) = y:(zipWithMaskTup xs ys)

--- Copy of zipWithMask that  produces errors on mismatched input instead of truncating
zipWithMask_errorOnMismatchedInput :: [a] -> [a] -> [Bool] -> [a]
zipWithMask_errorOnMismatchedInput [] [] [] = []
zipWithMask_errorOnMismatchedInput [] b c = error ("zipWithMask: Mismatched input 1: " ++ show (length b) ++ " and " ++  show(length c) ++ " left.")
zipWithMask_errorOnMismatchedInput a [] c = error ("zipWithMask: Mismatched input 2: " ++ show (length a) ++ " and " ++  show(length c) ++ " left.")
zipWithMask_errorOnMismatchedInput a b [] = error ("zipWithMask: Mismatched input 3: " ++ show (length a) ++ " and " ++  show(length b) ++ " left.")
zipWithMask_errorOnMismatchedInput (x:xs) (y:ys) (False:masks) = x:(zipWithMask_errorOnMismatchedInput xs ys masks)
zipWithMask_errorOnMismatchedInput (x:xs) (y:ys) (True:masks) = y:(zipWithMask_errorOnMismatchedInput xs ys masks)

zipWithMaskTup_errorOnMismatchedInput :: [a] -> [(a, Bool)] -> [a]
zipWithMaskTup_errorOnMismatchedInput [] [] = []
zipWithMaskTup_errorOnMismatchedInput [] a = error ("zipWithMask: Mismatched input 1: " ++ show (length a) ++ " left.")
zipWithMaskTup_errorOnMismatchedInput a [] = error ("zipWithMask: Mismatched input 2: " ++ show (length a) ++ " left.")
zipWithMaskTup_errorOnMismatchedInput (x:xs) ((y, False):ys) = x:(zipWithMaskTup_errorOnMismatchedInput xs ys)
zipWithMaskTup_errorOnMismatchedInput (x:xs) ((y, True):ys) = y:(zipWithMaskTup_errorOnMismatchedInput xs ys)


sumToInt :: Integral a => [a] -> Int
sumToInt [] = 0
sumToInt words = foldl (\s w -> s + fromIntegral w) 0 words

sumWord8ToInt_signed :: [Word8] -> Int
sumWord8ToInt_signed [] = 0
sumWord8ToInt_signed words = foldl (\s w -> s + word8ToInt_signed w) 0 words

word8ToInt_signed :: Word8 -> Int
word8ToInt_signed w
  | w < 128 = fromIntegral w
  | otherwise = fromIntegral w - 256

randomListElement :: R.RandomGen g => [a] -> g -> a
randomListElement [] _ = error "Requested random element of an empty list."
randomListElement list gen = list !! fst (R.randomR (0, length list - 1) gen)

replaceAtIndex :: [a] -> a -> Int -> [a]
replaceAtIndex list element index = (take index list) ++ element : drop (index+1) list

--- Warning Supression
{-# ANN awaitChar "HLint: ignore Use String" #-}
{-# ANN awaitChar2 "HLint: ignore Use String" #-}
{-# ANN randomR_Word8 "HLint: ignore Use camelCase" #-}
{-# ANN zipWithMask "HLint: ignore Redundant bracket" #-}
{-# ANN zipWithMaskTup "HLint: ignore Redundant bracket" #-}
{-# ANN zipWithMask_errorOnMismatchedInput "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Use camelCase" #-}