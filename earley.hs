module Earley
    (
      getRule
    ) where

import Data.List
import Data.Maybe
import Data.Char

data Token = Token String String

data Symbol = SymLiteral String | SymClass String | SymNonterminal String
    deriving (Eq)

instance Show Symbol where
    show (SymLiteral value) = '\'' : (value ++ "\'")
    show (SymClass value) = '[' : (value ++ "]")
    show (SymNonterminal value) = value

getValue :: Symbol -> String
getValue (SymLiteral s) = s
getValue (SymClass s) = s
getValue (SymNonterminal s) = s

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

appendDistinct :: Eq a => [a] -> [a] -> [a]
appendDistinct new l = l ++ [x | x <- new, not(x `elem` l)]

advance :: Int -> Item -> Item
advance n i = Item { rule = (rule i)
                   , dot = (dot i + n)
                   , origin = (origin i)
                   }

firstStateSet startRuleName grammar =
    map genItem [rule | rule <- grammar, name rule == startRuleName]
    where
        genItem rule = Item rule 0 0

 -- prediction
newItems :: Item -> [Rule] -> [Item]
newItems item grammar
    | not(check relevantSymbol) = []
    | otherwise = map
        (genItem item)
        [r | r <- grammar, name r == getValue relevantSymbol]
        where
            check (SymClass _) = False
            check (SymLiteral _) = False
            check (SymNonterminal _) = True
            genItem i rule = Item rule 0 (origin i)
            relevantSymbol = (symbols $ rule item) !! dot item

predictEach :: [Item] -> [Rule] -> [Item]
predictEach stateSet grammar =
    predictEach' stateSet grammar 0
    where
        predictEach' stateSet grammar position
            | position == (length stateSet - 1) && length news == 0 = stateSet
            | otherwise = predictEach' (stateSet ++ news) grammar (position + 1)
            where
                news = [i | i <- newItems', not(i `elem` stateSet)]
                newItems' = newItems cur grammar
                cur = stateSet !! position

 -- scanning
matches :: Symbol -> Token -> Bool
matches (SymNonterminal value) (Token t v) = value == v
matches (SymLiteral value) (Token t v) = value == v
matches (SymClass value) (Token t v) = charsIn v value
    where
        charsIn (v:vs) chClass
            | v `elem` (expand chClass) = charsIn vs chClass
            | otherwise = False
        charsIn [] chClass = True

expand :: String -> String
expand chClass = expand' "" False chClass
    where
        expand' temp True [] = temp ++ "-"
        expand' temp False [] = temp
        expand' temp flag ('-':xs) = expand' temp True xs
        expand' temp True (x:xs)
            | sameFullClass temp x = (expanded temp x) ++ expand' "" False xs
            | otherwise = temp ++ "-" ++ expand' [x] False xs
            where
                classes = [gen 'A' 'Z', gen 'a' 'z', gen '0' '9']
                gen start end
                    | start == end = [end]
                    | otherwise = start : gen (chr (ord start + 1)) end
                sameFullClass temp x = sameFullClass' tempClass x
                sameFullClass' Nothing x = False
                sameFullClass' c x = c == findClass classes x
                findClass [] ch = Nothing
                findClass (x:xs) ch
                    | ch `elem` x = Just x
                    | otherwise = findClass xs ch
                tempClass = findClass classes $ head temp
                expanded temp x = takeWhile (<= x) suffix
                suffix = dropWhile (< (head temp)) (fromJust tempClass)
        expand' temp False (x:xs) = temp ++ expand' [x] False xs

{-scanEach :: [Item] -> [Rule] -> Token -> [[Item]]
scanEach stateSet grammar curToken =
    scanEach' stateSet grammar 0 curToken
    where
        scanEach' stateSet grammar position curToken
            | position == (length stateSet - 1) =
            | not(isTerminal relevantSymbol) =
                scanEach' stateSet grammar (position + 1) curToken
            | otherwise = (getNew relevantSymbol curToken) :
                (scanEach' stateSet grammar (position + 1) curToken)
        relevantSymbol = (symbols $ rule item) !! dot item
        where
            item = stateSet !! position
        getNew symbol token
            |-}

-- scanEach gives the next state set
-- which is the set of earley items up to a terminal
-- from the current state set
-- that match the next input token


 -- test stuff
testFunc = predictEach stateSet myGrammar2

myGrammar = [ getRule "S" ["'a'", "S", "'b'"]
            , getRule "S" ["'ba'"]
            ]

myGrammar2 = [ getRule "Sum" ["Sum", "[+-]", "Product"]
             , getRule "Sum" ["Product"]
             , getRule "Product" ["Product", "[*/]", "Factor"]
             , getRule "Product" ["Factor"]
             , getRule "Factor" ["'('", "Sum", "')'"]
             , getRule "Factor" ["Number"]
             , getRule "Number" ["[0-9]", "Number"]
             , getRule "Number" ["[0-9]"]
             ]

stateSet = [ (Item (head myGrammar2) 2 0) ]

 -- recogniser
