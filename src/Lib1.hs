{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State(..), emptyState, gameStart, render, mkCheck, toggle, hint, GameRow(..), CellType(..), dynamicMap, dmapGet, deList, replaceRow
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is

-- Data definitions

data GameRow = GameDataRow [CellType] Int Int | GameSeparatorRow | ColumnInfoRow [Int] deriving (Eq, Show)
newtype State = State [GameRow] deriving (Eq, Show)
data CellType = EmptyCell | MarkedCell | HintCell deriving (Eq)

instance Show CellType where
    show EmptyCell = " "
    show MarkedCell = "X"
    show HintCell = "H"

-- Overrides


-- Helper functions

stringifyGameRow :: GameRow -> String
stringifyGameRow GameSeparatorRow = "\t+---+---+---+---+---+---+---+---+---+---+"
stringifyGameRow (GameDataRow [] _ r) = "| " ++ "(" ++ show r ++ ")"
stringifyGameRow (GameDataRow a l r)
    | l > 0 = show l ++ "\t| "++ show (head a) ++" " ++ stringifyGameRow (GameDataRow (tail a) 0 r)
    | otherwise = "| " ++ show (head a) ++" " ++ stringifyGameRow (GameDataRow (tail a) l r)
stringifyGameRow (ColumnInfoRow l)
    | length l == 10 = "\t" ++ recc
    | not (null l) = recc
    | otherwise = ""
    where recc = " (" ++ show (head l) ++ ")" ++ stringifyGameRow (ColumnInfoRow (tail l) )

stringifyFirstRow :: GameRow -> String
stringifyFirstRow (ColumnInfoRow l)
    | length l == 10 = "\t" ++ recc
    | not (null l) = recc
    | otherwise = ""
    where recc = "  " ++ show (head l) ++ " " ++ stringifyFirstRow (ColumnInfoRow (tail l) )
stringifyFirstRow _ = ""

dmapGet :: Document -> String -> Document
dmapGet (DMap ((s,d):xs)) key
    | s == key = d
    | otherwise = dmapGet (DMap xs) key
dmapGet _ _ = DNull

setOccupiedRow :: GameRow -> Document -> GameRow
setOccupiedRow (GameDataRow r acc _) (DInteger value) = GameDataRow r acc value
setOccupiedRow _ _ = GameSeparatorRow

dynamicMap :: (a -> b -> c) -> [a] -> [b] -> [c]
dynamicMap f (ax:axs) (bx:bxs) = f ax bx : dynamicMap f axs bxs
dynamicMap _ [] _ = []
dynamicMap _ _ [] = []

deList :: Document -> [Document]
deList (DList l) = l
deList _ = []

separated :: [GameRow] -> [GameRow]
separated (x:xs) = GameSeparatorRow : x : separated xs
separated [] = []

convertDocToInt :: Document -> Int
convertDocToInt (DInteger i) = i
convertDocToInt _ = 0

replaceRow :: (CellType -> CellType) -> Int -> Int-> [GameRow] -> [GameRow]
replaceRow logicFun r c xs = as ++  [ro] ++ tail bs
    where   ro = newRow logicFun (xs!!(r-1)) c
            (as, bs) = splitAt (r-1) xs


newRow :: (CellType -> CellType) -> GameRow->  Int -> GameRow
newRow  logicFun (GameDataRow array l r ) pos
    | array!!(pos-1) == EmptyCell = GameDataRow (replace pos (logicFun EmptyCell) array) l r
    | array!!(pos-1) == MarkedCell = GameDataRow (replace pos (logicFun MarkedCell) array) l r
    | otherwise = GameDataRow array l r
newRow _ a _ = a

toggleLogic :: CellType -> CellType
toggleLogic EmptyCell = MarkedCell
toggleLogic MarkedCell = EmptyCell
toggleLogic s = s

replace :: Int -> CellType -> [CellType] -> [CellType]
replace pos newVal list = as ++ [newVal] ++ tail bs
    where   (as, bs) = splitAt (pos-1) list

rowIterator :: [GameRow] -> Int -> Int -> [Coord] -> [Coord] -- iterates through whole board
rowIterator list n i iteratedList =
    if i == n
    then iteratedList
    else rowIterator (tail list) n (i+1) (iteratedList ++ columnIterator (head list) 10 0 [])

columnIterator :: GameRow -> Int -> Int -> [Coord] -> [Coord] -- iterates through each element of a row
columnIterator (GameDataRow cellTypes y var) n i iteratedList =
    if i == n
    then iteratedList
    else columnIterator (GameDataRow (tail cellTypes) y var) n (i+1) (updateList (iteratedList) (head cellTypes) (i) (y-1) )
columnIterator _ _ _ _ = undefined

updateList :: [Coord] -> CellType -> Int -> Int -> [Coord] -- helper function to check if a cell is marked and if so, add it to the rest of coords
updateList iteratedList cell x y =
    if isCellMarked cell == True
    then ((iteratedList) ++ [Coord {col = x, row = y}])
    else iteratedList

isCellMarked :: CellType -> Bool
isCellMarked EmptyCell = False
isCellMarked MarkedCell = True
isCellMarked HintCell = True

-- Functions to implement

-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState = State $ aux 10 0
    where
        aux n acc
         | n == acc   = [ColumnInfoRow [0,0,0,0,0,0,0,0,0,0]]
         | otherwise  = GameDataRow [EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,
                                    EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell] (acc+1) 0 : aux n (acc+1)



-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart (State s) d = State $ dynamicMap setOccupiedRow s (deList (dmapGet d "occupied_rows")) ++ [ColumnInfoRow (map convertDocToInt (deList (dmapGet d "occupied_cols")))]

-- IMPLEMENT
-- renders your game board
render :: State -> String
render (State s) = unlines $ stringifyFirstRow (ColumnInfoRow  [1,2,3,4,5,6,7,8,9,10]) : map stringifyGameRow (separated s)

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck (State s) = Check {coords = (rowIterator s 10 0 [])}

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle (State s) ["0","0"] = State s
toggle (State s) ["0", _ ] = State s
toggle (State s) [ _ ,"0"] = State s
toggle (State s) [ r , c ]
    | (read r::Int) > 10 = State s
    | (read c::Int) > 10 = State s
    | otherwise = State $ replaceRow toggleLogic (read r::Int) (read c::Int) s
toggle (State s) _ = State s

-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint s doc = hint' s (dmapGet doc "coords")

hint' :: State -> Document -> State
hint' (State s) hintList
    | dmapGet hintList "tail" /= DNull = hint' modifyStep (dmapGet hintList "tail")
    | otherwise = modifyStep
    where
        modifyStep = State $ replaceRow (const HintCell) (convertDocToInt (dmapGet cell "row") + 1) (convertDocToInt (dmapGet cell "col") + 1) s
        cell = dmapGet hintList "head"