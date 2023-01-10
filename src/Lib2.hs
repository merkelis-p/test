{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2(renderDocument, hint, gameStart, convertDocToInt, renderTabs, stripFrontWhitespace) where

import Types ( ToDocument(..), Document (DList, DMap, DInteger, DNull, DString), Check(..), Coord(..) )
import Lib1 (State(..), GameRow(..), CellType(..))

-- IMPLEMENT
-- First, make Check an instance of ToDocument class
instance ToDocument Check where
        toDocument (Check a) = DMap [("coords", DList $ map toDocument a)]

instance ToDocument Coord where
    toDocument (Coord c r) = DMap [("col", DInteger c), ("row", DInteger r)]

class ToYaml a where
    toyaml :: a -> String



-- IMPLEMENT
-- Renders document to yaml
renderDocument :: Document -> String
renderDocument d = "---\n" ++ toyaml d


instance ToYaml Document where
    toyaml (DMap []) = "{}\n"
    toyaml (DList []) = "[]\n"
    toyaml (DMap m) = unlines $ mapToYaml 0 (DMap m)
    toyaml (DList l) = unlines $ listToYaml 0 l
    toyaml d = yamlPrimitives d ++ "\n"


yamlPrimitives :: Document -> String
yamlPrimitives (DList []) = "[]"
yamlPrimitives (DMap []) = "{}"
yamlPrimitives (DString "") = "\"\""
yamlPrimitives (DString s) = '\'' : concatMap (\a -> if a == '\'' then "''" else [a]) s ++ ['\'']
--   | ':' `elem` s = show s
--   | all isDigit s = show s
--   | otherwise = s
yamlPrimitives (DInteger i) = show i
yamlPrimitives DNull = "null"

mapToYaml :: Int -> Document -> [String]
mapToYaml level (DMap ((key, DList []) : r))  = (renderTabs level ++ yamlPrimitives (DString key) ++ ": []" ) : mapToYaml level (DMap r)
mapToYaml level (DMap ((key, DMap []) : r))  = (renderTabs level ++ yamlPrimitives (DString key) ++ ": {}" ) : mapToYaml level (DMap r)
mapToYaml level (DMap ((key, DList l) : r)) = ((renderTabs level ++ yamlPrimitives (DString key) ++ ":") : listToYaml level  l) ++ mapToYaml level (DMap r)
mapToYaml level (DMap ((key, DMap m) : r))  = ((renderTabs level ++ yamlPrimitives (DString key) ++ ":") : mapToYaml (level+1) (DMap m)) ++ mapToYaml level (DMap r)
mapToYaml level (DMap ((key, value) : r))  = (renderTabs level ++ yamlPrimitives (DString key) ++ ": " ++ yamlPrimitives value ) : mapToYaml level (DMap r)
mapToYaml _ (DMap []) = []

listToYaml :: Int -> [Document] -> [String]
listToYaml level  ((DList []):lx) = (renderTabs level ++ "- []") : listToYaml level lx
listToYaml level  ((DList l):lx) = ((renderTabs level ++ "- ") : listToYaml (level+1) l) ++ listToYaml level lx
listToYaml level  ((DMap []):lx) =  (renderTabs level ++ "- {}") : listToYaml level lx
listToYaml level  ((DMap m):lx) =  squashTopRow ((renderTabs level ++ "- ") : mapToYaml (level+1) (DMap m)) ++ listToYaml level lx
listToYaml level  (l:lx) = (renderTabs level ++ "- " ++ yamlPrimitives l) : listToYaml level lx
listToYaml _ [] = []

squashTopRow ::  [String] -> [String]
squashTopRow (s:n:xs) = (s ++ stripFrontWhitespace n) : xs
squashTopRow [s] = [s]

stripFrontWhitespace :: String -> String
stripFrontWhitespace (c:s)
    | c == ' ' = stripFrontWhitespace s
    | otherwise = c:s
stripFrontWhitespace "" = ""

renderTabs :: Int -> String
renderTabs level
    | level < 1 = ""
    | otherwise = "  " ++ renderTabs (level - 1)


-- IMPLEMENT
-- This adds game data to initial state
-- Errors are reported via Either but not error 
gameStart :: State -> Document -> Either String State
gameStart (State s) d = do
    extracted_occ_rows <- dmapGet d "occupied_rows"
    extracted_occ_cols <- dmapGet d "occupied_cols"
    extracted_occ_rows' <- deList extracted_occ_rows
    extracted_occ_cols' <- deList extracted_occ_cols
    occ_rows <- dynamicMap setOccupiedRow s extracted_occ_rows'
    occ_rows' <- checkListForLefts occ_rows
    occ_cols <- checkListForLefts $ map convertDocToInt extracted_occ_cols'
    return $ State $ occ_rows' ++ [ColumnInfoRow occ_cols]


-- IMPLEMENT
-- Adds hint data to the game state
-- Errors are reported via Either but not error 
hint :: State -> Document -> Either String State
hint s doc = do
    x <- dmapGet doc "coords"
    hint' s x

hint' :: State -> Document -> Either String State
hint' (State s) hintList
    | dmapGet hintList "tail" /= Right DNull = do
        x <- dmapGet hintList "tail"
        ms <- modifyStep
        hint' ms x
    | otherwise = modifyStep
    where
        modifyStep :: Either String State
        modifyStep = do
            cell <- dmapGet hintList "head"
            r <- dmapGet cell "row"
            c <- dmapGet cell "col"
            x <- convertDocToInt r
            y <- convertDocToInt c
            return $ State $ replaceRow (const HintCell) (x+1) (y+1) s

-- Helper functions

checkListForLefts :: [Either err a] -> Either err [a]
checkListForLefts [] = Right []
checkListForLefts (x:xs) = do
    x' <- x
    y <- checkListForLefts xs
    return $ x' : y


dmapGet :: Document -> String -> Either String Document
dmapGet (DMap ((s,d):xs)) key
    | s == key = Right d
    | otherwise = dmapGet (DMap xs) key
dmapGet _ _ = Left "Not a map"

setOccupiedRow :: GameRow -> Document -> Either String GameRow
setOccupiedRow (GameDataRow r acc _) (DInteger value) = Right $ GameDataRow r acc value
setOccupiedRow _ _ = Left "No input provided"

dynamicMap :: (a -> b -> c) -> [a] -> [b] -> Either String [c]
dynamicMap f (ax:axs) (bx:bxs) = do
    x <- dynamicMap f axs bxs
    return $ f ax bx : x
dynamicMap _ [] _ = Right []
dynamicMap _ _ [] = Right []

deList :: Document -> Either String [Document]
deList (DList l) = Right l
deList _ = Left "Not a list"

separated :: [GameRow] -> [GameRow]
separated (x:xs) = GameSeparatorRow : x : separated xs
separated [] = []

convertDocToInt :: Document -> Either String Int
convertDocToInt (DInteger i) = Right i
convertDocToInt _ = Left "Not an integer"

replaceRow :: (CellType -> CellType) -> Int -> Int-> [GameRow] -> [GameRow]
replaceRow logicFun r c xs = as ++ [ro] ++ tail bs
    where   ro = newRow logicFun (xs!!(r-1)) c
            (as, bs) = splitAt (r-1) xs

newRow :: (CellType -> CellType) -> GameRow->  Int -> GameRow
newRow  logicFun (GameDataRow array l r ) pos
    | array!!(pos-1) == EmptyCell = GameDataRow (replace pos (logicFun EmptyCell) array) l r
    | array!!(pos-1) == MarkedCell = GameDataRow (replace pos (logicFun MarkedCell) array) l r
    | otherwise = GameDataRow array l r
newRow _ a _ = a


replace :: Int -> CellType -> [CellType] -> [CellType]
replace pos newVal list = as ++ [newVal] ++ tail bs
    where   (as, bs) = splitAt (pos-1) list

