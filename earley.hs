module Earley
    (
      getRule
    ) where

import Data.List
import Data.Maybe
import Data.Char

data Token = Token String String

instance Show Token where
    show (Token t v) = "(" ++ t ++ "," ++ v ++ ")"

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

isFinished :: Item -> Bool
isFinished i = (length . symbols . rule) i == dot i

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
newItems :: Item -> [Rule] -> Int -> [Item]
newItems item grammar curStateIndex
    | isFinished item || not(check relevantSymbol) = []
    | otherwise = map
        (genItem item)
        [r | r <- grammar, name r == getValue relevantSymbol]
        where
            check (SymClass _) = False
            check (SymLiteral _) = False
            check (SymNonterminal _) = True
            genItem i rule = Item rule 0 curStateIndex
            relevantSymbol = (symbols $ rule item) !! dot item

predictEach' :: [Item] -> [Rule] -> Int -> Int -> [Item]
predictEach' stateSet grammar position csi
    | position == (length stateSet - 1) && length news == 0 = stateSet
    | otherwise = predictEach' (stateSet ++ news) grammar (position + 1) csi
    where
        news = [i | i <- newItems', not(i `elem` stateSet)]
        newItems' = newItems cur grammar csi
        cur = stateSet !! position

-- predictEach
-- returns the fully predicted form of the given state set
predictEach :: [Item] -> [Rule] -> Int -> [Item]
predictEach [] grammar curStateIndex = []
predictEach stateSet grammar curStateIndex =
    predictEach' stateSet grammar 0 curStateIndex

-- predict
-- wraps predictEach for use with the whole array
predict :: [[Item]] -> [Rule] -> [[Item]]
predict stateArray grammar =
    init stateArray ++ [predictEach (last stateArray) grammar (length stateArray - 1)]

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

-- scanEach
-- returns the *next* state set based on the scans
scanEach :: [Item] -> [Rule] -> Token -> [Item]
scanEach stateSet grammar curToken =
    scanEach' stateSet grammar 0 curToken
    where
        scanEach' stateSet grammar position curToken
            | position == length stateSet = []
            | isFinished item || not(isTerminal relevantSymbol) = next
            | relevantSymbol `matches` curToken =
                (advance 1 item) : next
            | otherwise = next
            where
                item = stateSet !! position
                relevantSymbol = (symbols $ rule item) !! dot item
                next = scanEach' stateSet grammar (position + 1) curToken
                isTerminal (SymNonterminal s) = False
                isTerminal _ = True

-- scan
-- wraps scanEach for use with the whole array
scan :: [[Item]] -> [Rule] -> Token -> [[Item]]
scan stateArray grammar curToken =
    stateArray ++ [scanEach (last stateArray) grammar curToken]

 -- completion

-- findOrigins
-- Retrieves the origin rule(s) from the whole array
-- for the given Item
findOrigins :: [[Item]] -> Item -> [Item]
findOrigins wholeArray item = origins item relevantSet
    where
        relevantSet = wholeArray !! origin item
        origins i [] = []
        origins i (x:xs)
            | i `originatesFrom` x = x : origins i xs
            | otherwise = origins i xs
        originatesFrom i x = (getValue $ current x) == (name $ rule i)
            where
                current x = (symbols . rule) x !! dot x


-- completeEach
-- Returns the given list, and appended to that
-- the advanced origin rules of any completed
-- rules in the given list.
-- Must be given both the current state set as
-- well as the whole array of sets
completeEach :: [Item] -> [Rule] -> [[Item]] -> [Item]
completeEach stateSet grammar wholeArray =
    completeEach' stateSet grammar wholeArray 0
    where
        completeEach' stateSet grammar wholeArray position
            | position == length stateSet = stateSet
            | isFinished item =
                completeEach' updated grammar (init wholeArray ++ [updated]) (position + 1)
            | otherwise = next
            where
                updated = stateSet ++ news
                news = map (advance 1) (findOrigins wholeArray item)
                item = stateSet !! position
                next = completeEach' stateSet grammar wholeArray (position + 1)
{-completeEach :: [Item] -> [Rule] -> [[Item]] -> [Item]
completeEach stateSet wholeArray grammar
    | tail stateSet == [] = []-}


-- complete
-- wraps completeEach for use with the whole array
complete :: [[Item]] -> [Rule] -> [[Item]]
complete stateArray grammar =
    init stateArray ++ [completeEach (last stateArray) grammar stateArray]

accepts :: [[Item]] -> String -> [Token] -> Bool
accepts stateArray startRuleName str
    | length stateArray == length str + 1 = topRuleCompleted
    | otherwise = False
    where
        topRuleCompleted = topRuleCompleted' (last stateArray) startRuleName
        topRuleCompleted' [] _ = False
        topRuleCompleted' (x:xs) startRuleName
            | name (rule x) == startRuleName
                && origin x == 0
                && isFinished x = True
            | otherwise = topRuleCompleted' xs startRuleName

-- parse
-- (If successful) returns a tuple containing
-- the completed list of Earley items and whether the
-- algorithm was successful
recognise :: [Rule] -> String -> [Token] -> ([[Item]], Bool)
recognise grammar startRuleName str = (result, successful)
    where
        successful = accepts result startRuleName str
        result = next [firstSet] grammar str
        firstSet = firstStateSet startRuleName grammar
        next stateArray grammar [] = stateArray
        next stateArray grammar (x:xs)
            | hasWork   = next (process stateArray grammar x) grammar xs
            | otherwise = stateArray
            where
                hasWork = length (last stateArray) > 0
        process stateArray grammar token =
            (complete' . scan' . predict') stateArray
            where
                complete' stateArray = complete stateArray grammar
                scan' stateArray = scan stateArray grammar token
                predict' stateArray = predict stateArray grammar


 -- test stuff
tokenise [] = []
tokenise (x:xs) = Token "TOKEN" [x] : tokenise xs

testTokens2 = tokenise "1*3"
testTokens = tokenise "ab"

test = recognise myGrammar "S" testTokens
test2 = recognise myGrammar2 "Sum" $ testTokens2

myFirstSet = firstStateSet "Sum" myGrammar2

myGrammar = [ getRule "S" ["'a'", "S", "'b'"]
            , getRule "S" ["'b'", "'a'"]
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

stateSet = [ (Item (head myGrammar2) 0 0) ]
