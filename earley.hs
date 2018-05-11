module Earley
    (
      getRule
    ) where

import Data.List
import Data.Maybe

data Symbol = SymLiteral String | SymClass String | SymNonterminal String
    deriving (Eq)

instance Show Symbol where
    show (SymLiteral value) = '\'' : (value ++ "\'")
    show (SymClass value) = '[' : (value ++ "]")
    show (SymNonterminal value) = value

data Rule = Rule
    { name :: String
    , symbols :: [Symbol]
    } deriving (Eq)

instance Show Rule where
    show (Rule name symbols) = name ++ " ->" ++ stringify symbols
        where
          stringify [] = ""
          stringify (x:xs) = " " ++ show x ++ (stringify xs)

data Item = Item
    { rule :: Rule
    , dot :: Int
    , origin :: Int
    } deriving (Eq)

instance Show Item where
    show (Item rule dot origin) = show (dottedRule dot rule) ++ " (" ++ show origin ++ ")"
        where
          dottedRule i rule = Rule (name rule) (insertDotAt i $ symbols rule)
          insertDotAt i syms = insertElement dotSym i syms
          dotSym = SymNonterminal [toEnum 183 :: Char]

getRule :: String -> [String] -> Rule
getRule name symbols = Rule name $ toSymbols symbols

toSymbols :: [String] -> [Symbol]
toSymbols = map toSymbol

toSymbol :: String -> Symbol
toSymbol (x:xs)
    | x == '\'' = SymLiteral $ init xs
    | x == '['  = SymClass $ init xs
    | otherwise = SymNonterminal (x:xs) -- no characters to discard in this case

split :: Eq a => a -> [a] -> [[a]]
split delimiter [] = [[]]
split delimiter l
    | elem delimiter l = first : (split delimiter $ tail $ second )
    | otherwise = [l]
    where
      (first, second) = splitAt (fromJust $ elemIndex delimiter l) l

insertElement :: Eq a => a -> Int -> [a] -> [a]
insertElement new position xs = insert' new position 0 xs
    where
      insert' new position current (x:xs)
          | position == current = new : (x:xs)
          | xs == [] = x : [new]
          | otherwise = x : (insert' new position (current + 1) xs)

addDistinct :: Eq a => a -> [a] -> [a]
addDistinct new l
  | new `elem` l = l
  | otherwise = l ++ [new]

advance :: Int -> Item -> Item
advance n i = Item { rule = (rule i)
                   , dot = (dot i + n)
                   , origin = (origin i)
                   }

firstStateSet startRuleName = [rule | rule <- myGrammar, name rule == startRuleName]


 -- test stuff
myGrammar = [ getRule "S" ["'a'", "S", "'b'"]
            , getRule "S" ["'ba'"]
            ]

numrule = getRule "Number" ["[0-9]", "\'.\'", "Number"]
numitem = Item numrule 2 0
numitem2 = Item numrule 0 0

 -- recogniser
