{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Lib4(parseDocument, GameStart(..), Hint(..), gameStart, hint, Lib4.toggle) where

import Types ( Document(..), FromDocument, fromDocument, Coord (..) )
import Lib1 (State(..), GameRow (GameDataRow, ColumnInfoRow), dynamicMap, dmapGet, deList, replaceRow, CellType(..), toggle)
import Lib2 (renderTabs, stripFrontWhitespace, convertDocToInt)
import Data.Char (isDigit)
import Control.Arrow (Arrow(first, second))
import Control.Lens (Bifunctor(bimap))

-- IMPLEMENT
-- Parses a document from yaml
parseDocument :: String -> Either String Document
parseDocument str = do
    starter <- parseStringExact "---\n" str `Lib4.or` parseStringExact "" str
    parsed <- parseComplexDoc (snd starter) `Lib4.or` parsePrimitiveDoc (snd starter)
    return (fst parsed)
    -- if null (snd parsed) then Right (fst parsed) else Left ("Error parsing near" ++ snd parsed)

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart [Int] [Int] deriving (Show, Eq)

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint [Coord] deriving (Show, Eq)

instance FromDocument GameStart where
    fromDocument d = validateGameStart d >> Right (GameStart (map (\(DInteger a) -> a) (deList (dmapGet d "occupied_rows"))) (map (\(DInteger a) -> a) (deList (dmapGet d "occupied_cols"))))

instance FromDocument Hint where
    fromDocument d = validateHint d >> Right (Hint $ [ Coord {row = (\(DInteger a) -> a) (dmapGet x "row"), col = (\(DInteger a) -> a)(dmapGet x "col")} | x <- recToList (dmapGet d "coords")])

toggle :: State -> Document -> Either String State
toggle initial (DList (x:xs)) = do
    row <- show . (+1) <$> convertDocToInt (dmapGet x "row")
    col <- show . (+1) <$> convertDocToInt (dmapGet x "col")
    Lib4.toggle (Lib1.toggle initial [row,col]) (DList xs)
toggle initial _ = return initial

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
gameStart :: State -> GameStart -> State
-- gameStart (State l) d = State $ ("Game started: " ++ show d) : l
gameStart (State existingRows) (GameStart occ_rows occ_cols) = State $ dynamicMap (\(GameDataRow r left_val _) right_val -> GameDataRow r left_val right_val) existingRows occ_rows ++ [ColumnInfoRow occ_cols]

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
hint :: State -> Hint -> State
-- hint (State l) h = State $ ("Hint " ++ show h) : l
hint (State s) (Hint (h:hx)) = hint (State (replaceRow (const HintCell) (row h + 1) (col h + 1) s)) (Hint hx)
hint s (Hint []) = s

validateHint :: Document -> Either String Bool
validateHint (DMap []) = Left "Hint: expected DMap not empty"
validateHint (DMap [("coords", doc)]) = validateHint doc
validateHint
  (DMap [(strHead, DMap [(strCol, c), (strRow, r)]), (strTail, doc)])
  | strHead /= "head" = Left $ "Hint: expected \"head\", but got: \"" ++ strHead ++ "\""
  | strCol /= "col" = Left $ "Hint: expected \"col\", but got: \"" ++ strCol ++ "\""
  | strRow /= "row" = Left $ "Hint: expected \"row\", but got: \"" ++ strRow ++ "\""
  | strTail /= "tail" = Left $ "Hint: expected \"tail\", but got: \"" ++ strTail ++ "\""
  | otherwise = validateDInteger c h 9 >>
   validateDInteger r h 9 >>
   if doc == DNull then Right True else validateHint doc
  where
      h = "Hint: "
validateHint _ = Left "Hint: Invalid Hint document"

validateGameStart :: Document -> Either String Bool
validateGameStart d = validateList (deList (dmapGet d "occupied_cols")) h >>
                    validateList (deList (dmapGet d "occupied_rows")) h >>
                    return True
    where
        h = "GameStart: "

validateDInteger :: Document -> String -> Int -> Either String Bool
validateDInteger (DInteger i) h m =
    if i > m || i < 0
        then Left $ h ++ "Invalid scope of DInteger: " ++ show i
            else Right True
validateDInteger doc h _ = Left $ h ++ "expected DInteger, but got " ++ (show doc)

validateList :: [Document] -> String -> Either String Bool
validateList [] h = Left $ h ++ "Invalid GameStart Document (No \"row\" or \"col\" keys)"
validateList (x:xs) h = do
    y <- validateDInteger x h 10
    if null xs then Right y else validateList xs h

recToList :: Document -> [Document]
recToList d
    | dmapGet d "tail" /= DNull = dmapGet d "head" : recToList (dmapGet d "tail")
    | otherwise = [dmapGet d "head"]


-- The parser part

or :: Either String (a, String)
    -> Either String (a, String)
    -> Either String (a, String)
or parser1 parser2 =
    case parser1 of
        Right a -> Right a
        Left _ -> parser2

parseChar :: Char -> String -> Either String (Char, String)
parseChar ch [] = Left $ "Empty input: '" ++ [ch] ++ "' expected"
parseChar ch (x:xs) | ch == x = Right (x, xs)
                    | otherwise = Left $ ch :" expected"

parseInteger :: String -> Either String (Int, String)
parseInteger str = do
    num <-
        let
        prefix = takeWhile isDigit str
        in
        case prefix of
            [] -> Left "Empty integer"
            _ -> Right (read prefix, drop (length prefix) str)
    rest <- parseNL (stripFrontWhitespace (snd num))
    return (fst num, snd rest)

parseNegativeInteger :: String -> Either String (Int, String)
parseNegativeInteger str = do
    neg <- parseChar '-' str
    num <- parseInteger (snd neg)
    return (-1*fst num, snd num)

parseString :: String -> Char -> Either String (String, String)
parseString (s:sx) delim =
    if s == delim
    then Right ("", sx)
    else do
        x <- parseString sx delim
        return $ (s :) <$> x
parseString [] _ = Left "Nothing Provided"

parseStringWithDelim :: Char -> String -> Either String (String, String)
parseStringWithDelim delim (s:sx)
  | s == '\'' && (head sx == '\'') = first ('\'' :) <$> parseStringWithDelim delim (tail sx)
  | s == delim = Right ("", sx)
  | otherwise = do
        x <- parseStringWithDelim delim sx
        return $ first (s :) x
parseStringWithDelim _ [] = Left "Delimiter was not found in string"

parseStringWithDelimNoEscape :: Char -> String -> Either String (String, String)
parseStringWithDelimNoEscape delim (s:sx)
  | s == delim = Right ("", sx)
  | otherwise = do
        x <- parseStringWithDelimNoEscape delim sx
        return $ first (s :) x
parseStringWithDelimNoEscape _ [] = Left "Delimiter was not found in string"

parseStringWithDelimKeep :: Char -> String -> Either String (String, String)
parseStringWithDelimKeep delim str = first (++ [delim]) <$> parseStringWithDelim delim str

parseStringWithDelimKeepNoEscape :: Char -> String -> Either String (String, String)
parseStringWithDelimKeepNoEscape delim str = first (++ [delim]) <$> parseStringWithDelimNoEscape delim str

parseQuotedString :: String -> Either String (String, String)
parseQuotedString str = do
    quote <- parseChar '\'' str `Lib4.or` parseChar '"' str `Lib4.or` parseChar '`' str
    uncurry parseStringWithDelim quote

parseStringExact :: String -> String -> Either String (String, String)
parseStringExact expected given =
    if take (length expected) given == expected
    then Right (expected, drop (length expected) given)
    else Left ("Expected " ++ expected ++ " but found " ++ take (length expected) given)

parseEmptyString :: String -> Either String (String, String)
parseEmptyString str = parseStringExact "\"\"" str `Lib4.or` parseStringExact "\'\'" str

parseEmptyList :: String -> Either String (String, String)
parseEmptyList = parseStringExact "[]"

parseEmptyMap :: String -> Either String (String, String)
parseEmptyMap = parseStringExact "{}"

parseNull :: String -> Either String (String, String)
parseNull str = (parseStringExact "null" str `Lib4.or` parseStringExact "~" str) >>= (\(i,r) -> if null r then return (i,r) else parseNL r >> return (i,r))

parseTab :: Int -> String -> Either String (Integer, String)
parseTab level str
    | level == 0 = Right (0, str)
    | otherwise = do
        x <- parseStringExact "  " str
        parseTab (level-1) (snd x)

parseNL :: String -> Either String (Char, String)
parseNL = parseChar '\n'

parseAllString :: String -> Either String (Document, String)
parseAllString str = Right (DString str, "")

parsePrimitiveDoc :: String -> Either String (Document, String)
parsePrimitiveDoc str =
    ((DNull,).snd <$> parseNull str) `Lib4.or`
    (first DInteger <$> parseInteger str) `Lib4.or`
    (first DInteger <$> parseNegativeInteger str) `Lib4.or`
    ((DList [],).snd <$> parseEmptyList str) `Lib4.or`
    ((DMap [],).snd <$> parseEmptyMap str) `Lib4.or`
    (bimap DString ('\n' :) <$> parseQuotedString str) `Lib4.or`
    -- ((DString "",).snd <$> parseEmptyString str) `Lib4.or`
    (bimap DString ('\n' :) <$> parseStringWithDelimNoEscape '\n' str) `Lib4.or`
    parseAllString str

parseComplexDoc :: String -> Either String (Document, String)
parseComplexDoc str = parseList 0 str `Lib4.or` parseMap 0 str


parseMap :: Int -> String -> Either String (Document, String)
parseMap level str = do
    tabs <- parseTab level str
    element <- parseMapItemList level (snd tabs) `Lib4.or`
               parseMapItemMap level (snd tabs) `Lib4.or`
               parseMapItemSimple (snd tabs)
    nextItems <- case parseMap level (snd element) of
        Left _ -> Right (DMap [], snd element)
        Right (dmap, r) -> Right (dmap, r)
    return ( (\(DMap a) -> DMap (fst element:a)) (fst nextItems) , snd nextItems )

getMapKey :: (String, b) -> Either String (String, String)
getMapKey line =
            (parseQuotedString (fst line) >>= (\(i,r) -> (i,).snd <$> parseChar ':' r)) `Lib4.or`
            (parseEmptyString (fst line) >>= (\(i,r) -> (i,).snd <$> parseChar ':' r)) `Lib4.or`
            case parseQuotedString (fst line) of
                Left _ ->  parseStringWithDelimSeq ": " (fst line) `Lib4.or`
                           (second ('\n' :) <$> parseStringWithDelimSeq ":\n" (fst line))
                Right _ -> Left "String has ':' in it but is not a map key"

parseStringWithDelimSeq :: String -> String -> Either String (String, String)
parseStringWithDelimSeq (s:sx) str = do
    (i,r) <- parseStringWithDelim s str
    case parseStringExact sx r of
        Right (_, r2) -> return (i,r2)
        Left _ -> first ((i++[s]) ++) <$> parseStringWithDelimSeq (s:sx) r
parseStringWithDelimSeq "" _ = Left "Delimiter Sequence not found"


parseMapItemSimple :: String -> Either String ((String, Document), String)
parseMapItemSimple str = do
    line <- parseStringWithDelimKeepNoEscape '\n' str
    key <- getMapKey line
    value <- parsePrimitiveDoc ((stripFrontWhitespace.snd) key)
    return ((fst key, fst value), snd line)

parseMapItemMap :: Int -> String -> Either String ((String, Document), String)
parseMapItemMap level str = do
    line <- parseStringWithDelimKeepNoEscape '\n' str
    key <- getMapKey line
    _ <- parseNL $ snd key
    mapped <- parseMap (level + 1) $ snd line
    return ((fst key, fst mapped), snd mapped)

parseMapItemList :: Int -> String -> Either String ((String, Document), String)
parseMapItemList level str = do
    line <- parseStringWithDelimKeepNoEscape '\n' str
    key <- getMapKey line
    _ <- parseNL $ snd key
    list <- parseList (level+1) (snd line) `Lib4.or`
            parseList level (snd line)
    return ((fst key, fst list), snd list)


parseList :: Int -> String -> Either String (Document, String)
parseList level str = do
    tabs <- parseTab level str
    element <- parseListItemList level (snd tabs) `Lib4.or`
               parseListItemMap level (snd tabs) `Lib4.or`
               parseListItemSimple (snd tabs)
    nextItems <- case parseList level (snd element) of
        Left _ -> Right (DList [], snd element)
        Right (dlist, r) -> Right (dlist, r)
    return ( (\(DList a) -> DList (fst element:a)) (fst nextItems) , snd nextItems )


parseListItemSimple :: String -> Either String (Document, String)
parseListItemSimple str = do
    line <- parseStringWithDelimKeepNoEscape '\n' str
    dash <- parseStringExact "- " (fst line)
    item <- parsePrimitiveDoc (snd dash)
    return (fst item, snd line)

parseListItemMap :: Int -> String -> Either String (Document, String)
parseListItemMap level str = do
    line <- parseStringWithDelimKeepNoEscape '\n' str
    dash <- parseStringExact "- " (fst line)
    eol <- case parseNL (snd dash) of
        Left _ -> Right False
        Right _-> Right True
    parseMap (level+1) (if not eol then
        renderTabs (level+1) ++ stripFrontWhitespace (snd dash) ++ snd line
        else snd line)

parseListItemList :: Int -> String -> Either String (Document, String)
parseListItemList level str = do
    line <- parseStringWithDelimKeepNoEscape '\n' str
    dash <- parseStringExact "- " (fst line)
    case parseStringExact "- " (snd dash) of
        Left _ -> parseNL (snd dash) >> parseList (level+1) (snd line)
        Right _ -> parseList (level+1) (renderTabs (level+1) ++ snd dash ++ snd line)