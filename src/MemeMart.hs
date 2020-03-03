module MemeMart (
  memeMart
) where

import System.IO
import qualified Data.Char as C
import qualified Data.List as L
import qualified System.FilePath as FP
import GeneralUtility
import DatabaseParse
import Types

type Continue = Bool

memeMart :: DatabaseStruct -> IO Order
memeMart db = do
  putStrLn "Welcome to Meme Mart, how can i help you?~"
  putStrLn "Just enter your order, or \"exit\" to return to the Randomizer~"
  putStrLn "To see what we offer, look to Meme_Mart_Menu.txt or enter \"menu\"!~"
  putStrLn "Most orders are combinable with eachother~"
  putStrLn "We are case insensitive now!~"
  hFlush stdout

  input <- getLine
  let order = []
  let memebuffer = memeSelection order db (splitAtFirst '.' (map C.toLower input))
  hFlush stdout
  memeMartCont db memebuffer


memeMartCont :: DatabaseStruct -> (Order, Continue, String) -> IO Order
memeMartCont db (o, False, talk) = shopkeeper o talk
memeMartCont db (o, _, talk) = do
  order <- shopkeeper o talk
  putStrLn ""
  putStrLn "Anything else you want?~ Just enter an item from the menu~"
  putStrLn "If you are satisfied with your order, enter \"done\"~"
  putStrLn "If you wish to see your order so far, enter \"order\"~"
  putStrLn "If you wish to drop your order and leave, enter \"exit\"~"
  hFlush stdout

  input <- getLine
  memeMartCont db (memeSelection order db (splitAtFirst '.' (map C.toLower input)))


memeSelection :: Order -> DatabaseStruct -> (String, String) -> (Order, Continue, String)
--- Utility ---
memeSelection o db ("exit", _) = ([], False, "Bye bye, come back sometime!~\n")
memeSelection [] db  ("done", _) = ([], False, "Oh, leaving with nothing? Bye bye then!~\n")
memeSelection o db  ("done", _) = (o, False, "Have fun out there with that!~\n")
memeSelection [] db  ("order" ,_) = ([], True, "You haven't ordered anything yet!~")
memeSelection o db  ("order", _) = (o, True, "Your order currently looks like this:\n" ++ L.intercalate ", " (map (\x -> fst x ++ snd x) o))
memeSelection o db  ("orders", _) = memeSelection o db  ("order", [])
memeSelection o db  ("menu", _) = (o, True, "*")
--- Dispos
memeSelection o db  ("dragon.", arg) = dragonValidation o ("dragon.", arg)
memeSelection o db  ("dragons.", arg) = memeSelection o db  ("dragon.", arg)
--- Items
memeSelection o db  ("bridge", _) = (("allitem.","73"):o, True, "B R I D G E   K E Y")
memeSelection o db  ("69", _) = (("69", ""):o, True, "It's 69, you gotta do it!!")
memeSelection o db  ("allitem.", arg) = itemValidation o db ("allitem.", arg)
memeSelection o db  ("devil", _) = (("devil", ""):o, True, "Every weapon can backfire!")
memeSelection o db  ("brave", _) = (("brave", ""):o, True, "Every weapon will strike twice!")
--- Invalid order
memeSelection o db  other = (o, True, "Sorry, nothing like that in stock.~")



shopkeeper :: Order -> String -> IO Order
shopkeeper o "*" = printMenu o
shopkeeper o talk = do putStrLn talk
                       hFlush stdout
                       return o

printMenu :: Order -> IO Order
printMenu o = do
  dir <- getWorkingDirectory
  menu <- readFile (dir ++ FP.pathSeparator:"Meme_Mart_Menu.txt")
  putStrLn menu
  hFlush stdout
  return o

dragonValidation :: Order -> (String, String) -> (Order, Continue, String)
dragonValidation o (com, "me") = ((com , "me"):o, True, "Your units are all Dragons!")
dragonValidation o (com, "all") = ((com , "all"):o, True, "Everyone gets to be Dragons!")

itemValidation :: Order -> DatabaseStruct -> (String, String) -> (Order, Continue, String)
itemValidation o db (com, arg) = if isThisStringAnIntInRange itemNrRange arg
  then ((com , arg):o, True, "All items will now appear like " ++ dbIIDNameByItemNr db (read arg) ++ "s.")
  else (o, True, "\"" ++ arg ++ "\" is not a valid input for " ++ com ++ " i'm afraid~")

--- Arguments are a function that validates, an Order, a tuple of the command that takes an input and
--- the input given to be validated, and the message to give if the input is valid.
memeValidation :: (String -> Bool) -> Order -> (String, String) -> String -> (Order, Continue, String)
memeValidation validator o (command, arg) message = if validator arg
then ((command , arg):o, True, message)
else (o, True, "\"" ++ arg ++ "\" is not a valid input for " ++ command ++ " i'm afraid~")










--- Warning Supression
{-# ANN module "HLint: ignore Use <$>" #-}