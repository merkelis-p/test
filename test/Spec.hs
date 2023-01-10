import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.String.Conversions
import Data.Yaml as Y ( encodeWith, defaultEncodeOptions, defaultFormatOptions, setWidth, setFormat)

import Lib2
import Lib1 (emptyState, State(..), GameRow(..), CellType(..))
import Lib3 (parseDocument, Hint(..), GameStart(..))
import Types (Document(..), fromDocument, Coord(..))

main :: IO ()
main = defaultMain (testGroup "Tests" [
  toYamlTests,
  fromYamlTests,
  gameStartTests,
  hintTests,
  properties,
  fromDocumentGameStartTests,
  fromDocumentHintTests
  -- deeplyNestedTests,
  -- multiMapTests
  ])

properties :: TestTree
properties = testGroup "Properties" [golden, dogfood]

friendlyEncode :: Document -> String
friendlyEncode doc = cs (Y.encodeWith (setFormat (setWidth Nothing defaultFormatOptions) defaultEncodeOptions) doc)

golden :: TestTree
golden = testGroup "Handles foreign rendering"
  [
    testProperty "parseDocument (Data.Yaml.encode doc) == doc" $
      \doc -> parseDocument (friendlyEncode doc) == Right doc
  ]

dogfood :: TestTree
dogfood = testGroup "Eating your own dogfood"
  [
    testProperty "parseDocument (renderDocument doc) == doc" $
      \doc -> parseDocument (renderDocument doc) == Right doc
  ]

fromYamlTests :: TestTree
fromYamlTests = testGroup "Document from yaml"
  [   testCase "null" $
        parseDocument "~" @?= Right DNull
      , testCase "string" $
        parseDocument "---\n'string'\n" @?= Right (DString "string")
      , testCase "int" $
        parseDocument "---\n5\n" @?= Right (DInteger 5)
      , testCase "Negative int" $
          parseDocument "---\n-5\n" @?= Right (DInteger (-5))
      , testCase "List of ints" $
          parseDocument listOfInts @?= Right (DList [DInteger 5, DInteger 6])
      , testCase "Map" $
          parseDocument (unlines ["---", "'key1': 1", "'key2': 'string'", "'key3': null"]) @?= Right (DMap [("key1", DInteger 1), ("key2", DString "string"), ("key3", DNull)])
      , testCase "List of lists" $
          parseDocument listOfLists @?= Right (DList [DString "hello", DInteger 1, DList [DInteger 1, DString "level", DString "deeper"]])
      , testCase "List of lists of lists" $
          parseDocument listOfListsOfLists @?= Right (DList [DString "hello", DInteger 1, DList [DInteger 1, DList [DString "evendeeper", DInteger 5], DString "deeper"]])
      , testCase "List of Maps" $
          parseDocument listOfMaps @?= Right (DList [DMap [("row", DInteger 1), ("col", DInteger 2)]])
      , testCase "Map of list" $
          parseDocument mapOfList @?= Right (DMap [("coords", DList[DInteger 5, DInteger 6]), ("col", DInteger 2)])
      , testCase "Map of map" $
          parseDocument mapOfMap @?= Right (DMap [("coords", DMap[("row", DInteger 6), ("col", DInteger 5)]), ("random", DString "string")])
      , testCase "Scary Spooky Test" $
          parseDocument trickyCaseString @?= Right (trickyCaseDocument)
  ]

-- multiMapTests :: TestTree
-- multiMapTests =
--   let doc = DMap[("a", DInteger 1), ("a", DInteger 2)]
--   in
--   testGroup "MultiMap tests" [
--   testCase "dogfood" $ parseDocument (renderDocument doc) @?= Right doc,
--   testCase "golden" $ parseDocument (cs (friendlyEncode doc)) @?= Right doc--,
--   -- testCase "just Our output" $ renderDocument doc @?= "lol",
--   -- testCase "just Y output" $ cs (Y.encode doc) @?= "lol"
--   ]

-- deeplyNestedTests :: TestTree
-- deeplyNestedTests =
--   let doc = DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DList [DMap [("hGvuXzk",DString "D fQq5 G3M dv32")]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
--   in
--   testGroup "Deeply Nested tests" [
--   testCase "dogfood" $ parseDocument (renderDocument doc) @?= Right doc,
--   testCase "golden" $ parseDocument (cs (friendlyEncode doc)) @?= Right doc,
--   -- testCase "just Our output" $ renderDocument doc @?= "lol",
--   testCase "just Y output" $ cs (friendlyEncode doc) @?= "lol"
--   ]

toYamlTests :: TestTree
toYamlTests = testGroup "Document to yaml"
  [   testCase "null" $
        renderDocument DNull @?= "---\nnull\n"
    , testCase "string" $
        renderDocument (DString "string") @?= "---\n'string'\n"
    , testCase "int" $
        renderDocument (DInteger 5) @?= "---\n5\n"
    , testCase "Negative int" $
        renderDocument (DInteger (-5)) @?= "---\n-5\n"
    , testCase "List of ints" $
        renderDocument (DList [DInteger 5, DInteger 6]) @?= listOfInts
    , testCase "Map" $
    renderDocument (DMap [("key1", DInteger 1), ("key2", DString "string"), ("key3", DNull)]) @?= unlines ["---", "'key1': 1", "'key2': 'string'", "'key3': null"]
    , testCase "List of lists" $
    renderDocument (DList [DString "hello", DInteger 1, DList [DInteger 1, DString "level", DString "deeper"]]) @?= listOfLists
    , testCase "List of lists of lists" $
    renderDocument (DList [DString "hello", DInteger 1, DList [DInteger 1, DList [DString "evendeeper", DInteger 5], DString "deeper"]]) @?= listOfListsOfLists
    ,testCase "List of Maps" $
    renderDocument (DList [DMap [("row", DInteger 1), ("col", DInteger 2)]]) @?= listOfMaps
    ,testCase "Map of list" $
    renderDocument (DMap [("coords", DList[DInteger 5, DInteger 6]), ("col", DInteger 2)]) @?= mapOfList
    ,testCase "Map of map" $
    renderDocument (DMap [("coords", DMap[("row", DInteger 6), ("col", DInteger 5)]), ("random", DString "string")]) @?= mapOfMap
    , testCase "Scary Spooky Test" $
      renderDocument trickyCaseDocument @?= trickyCaseString
    , testCase "List of Empty Lists" $
      renderDocument (DList [DList [], DList [], DList [], DList[DList []], DList[
        DList [],
        DList [],
        DList []
      ]]) @?= listOfEmptyLists
    , testCase "Empty Map Key" $
      renderDocument (DMap [("", DString "Hello")]) @?= unlines ["---", "\"\": 'Hello'"]
    -- IMPLEMENT more test cases:
    -- * other primitive types/values
    -- * nested types
  ]

-- DMap [("occupied_rows", DList [ DInteger ...]), ("occupied_cols", DList [ DInteger ...])]

fromDocumentGameStartTests :: TestTree
fromDocumentGameStartTests = testGroup "fromDocument gameStart tests"
  [
      testCase "Document \"row\" and \"col\" key test" $
        getLeft ((fromDocument (DMap [("netinkamas", DList [DInteger 5]), ("occupied_cols", DList [DInteger 1])]))
        :: Either String GameStart)
          @?= "GameStart: Invalid GameStart Document (No \"row\" or \"col\" keys)"

    , testCase "Is list item Document DInteger test" $
        getLeft (fromDocument (DMap [("occupied_rows", DList [DNull]), ("occupied_cols", DList [DInteger 0])])
        :: Either String GameStart)
          @?= "GameStart: expected DInteger, but got DNull"

    , testCase "Document DInteger value scope test" $
        getLeft ((fromDocument (DMap [("occupied_rows", DList [DInteger 15]), ("occupied_cols", DList [DInteger 0])]))
        :: Either String GameStart)
          @?= "GameStart: Invalid scope of DInteger: 15"

    , testCase "GameStart happy, valid test" $
        getRight ((fromDocument (DMap [("occupied_rows", DList [DInteger 5]), ("occupied_cols", DList [DInteger 1])]))
        :: Either String GameStart)
          @?= GameStart [5] [1]
  ]

fromDocumentHintTests :: TestTree
fromDocumentHintTests = testGroup "fromDocument Hint tests"
  [
      testCase "Document DMap empty test" $
        getLeft ((fromDocument (DMap []))
        :: Either String Hint)
          @?= "Hint: expected DMap not empty"

    , testCase "\"coords\" key test" $
        getLeft ((fromDocument (DMap [("none", DNull)]))
        :: Either String Hint)
          @?= "Hint: Invalid Hint document"

    , testCase "\"head\" key test" $
        getLeft ((fromDocument (DMap [("coords",DMap [("none",DMap [("none", DInteger 1),("none", DInteger 1)]),("none", DNull)])]))
        :: Either String Hint)
          @?= "Hint: expected \"head\", but got: \"none\""

    , testCase "\"col\" key test" $
        getLeft ((fromDocument (DMap [("coords",DMap [("head",DMap [("none", DInteger 1),("none", DInteger 1)]),("none", DNull)])]))
        :: Either String Hint)
          @?= "Hint: expected \"col\", but got: \"none\""

    , testCase "\"row\" key test" $
        getLeft ((fromDocument (DMap [("coords",DMap [("head",DMap [("col", DInteger 1),("none", DInteger 1)]),("none", DNull)])]))
        :: Either String Hint)
          @?= "Hint: expected \"row\", but got: \"none\""

    , testCase "\"tail\" key test" $
        getLeft ((fromDocument (DMap [("coords",DMap [("head",DMap [("col", DInteger 1),("row", DInteger 1)]),("none", DNull)])]))
        :: Either String Hint)
          @?= "Hint: expected \"tail\", but got: \"none\""

    , testCase "Is list item Document DInteger test" $
        getLeft ((fromDocument (DMap [("coords",DMap [("head",DMap [("col", DString "aaa"),("row", DInteger 1)]),("tail", DNull)])]))
        :: Either String Hint)
          @?= "Hint: expected DInteger, but got DString \"aaa\""

    , testCase "DInteger scope test" $
        getLeft ((fromDocument (DMap [("coords",DMap [("head",DMap [("col", DInteger 45),("row", DInteger 1)]),("tail", DNull)])]))
        :: Either String Hint)
          @?= "Hint: Invalid scope of DInteger: 45"

    , testCase "Hint happy test" $
        getRight ((fromDocument (DMap [("coords",DMap [("head",DMap [("col", DInteger 6),("row", DInteger 2)]),("tail", DNull)])]))
        :: Either String Hint)
          @?= Hint [Coord {col = 6, row = 2}]
  ]

gameStartTests :: TestTree
gameStartTests = testGroup "Game start tests"
  [
      testCase "Rows Cols render error" $
        getLeft (gameStart emptyState DNull) @?= "Not a map"
    , testCase "DList not provided error" $
        getLeft (gameStart emptyState (DMap [("occupied_rows", DNull),("occupied_cols", DNull)])) @?= "Not a list"
    , testCase "GameStart bad row input error" $
        getLeft (gameStart emptyState (DMap [("occupied_rows", DList [DString "test1"]),("occupied_cols", DList [DString "test2"])])) @?= "No input provided"
    , testCase "GameStart bad State error" $
        getLeft (gameStart (State[GameSeparatorRow]) (DMap [("occupied_rows", DList [DInteger 5]),("occupied_cols", DList [DInteger 5])])) @?= "No input provided"
    , testCase "Error providing a document as a Dinteger" $
        getLeft (convertDocToInt (DMap [])) @?= "Not an integer"
    , testCase "gameStart was successful!" $
        getLeft (gameStart emptyState (DMap [("occupied_rows", DList [DInteger 5]),("occupied_cols", DList [DInteger 5])])) @?= "Expected Left"
  , testCase "gameStart Happy path Test 1: test of Return (State)" $
        getRight (gameStart emptyState (DMap [("occupied_rows", DList [DInteger 2]),("occupied_cols", DList [DInteger 5])])) @?= startStateTest1 2 5
  , testCase "gameStart Happy path Test 2:  test of Return (State)" $
        getRight (gameStart customState (DMap [("occupied_rows", DList [DInteger 1]),("occupied_cols", DList [DInteger 9])])) @?= startStateTest2 1 9
  ]

hintTests :: TestTree
hintTests = testGroup "Hint tests"
  [    testCase "dMapGet Error Test 1" $
        getLeft(hint emptyState DNull) @?= "Not a map"
      ,testCase "dMapGet Error Test 2" $
        getLeft(hint emptyState (DString "test")) @?= "Not a map"
      ,testCase "dMapGet Error Test 3" $
        getLeft(hint emptyState (DInteger 5)) @?= "Not a map"
      ,testCase "dMapGet Error Test 4" $
        getLeft(hint emptyState (DList [DInteger 5, DString "test", DNull])) @?= "Not a map"
      ,testCase "Deeper dMapGet Error Test" $
        getLeft(hint emptyState (DMap [("coords", DMap[("test", DInteger 5)])])) @?= "Not a map"
      ,testCase "convertDocToInt Error Test 1" $
        getLeft(hint emptyState (DMap [("coords", DMap[("head", DMap[("col", DInteger 5),("row", DNull)]),("tail", DNull)])])) @?= "Not an integer"
      ,testCase "convertDocToInt Error Test 2" $
        getLeft(hint emptyState (DMap [("coords", DMap[("head", DMap[("col", DNull),("row", DInteger 5)]),("tail", DNull)])])) @?= "Not an integer"
      ,testCase "Everything OK" $
        getLeft(hint emptyState (DMap [("coords", DMap[("head", DMap[("col", DInteger 5),("row", DInteger 5)]),("tail", DNull)])])) @?= "Expected Left"
      ,testCase "Hint Happy path Test1" $
        getRight(hint customState' (DMap [("coords", DMap[("head", DMap[("col", DInteger 0),("row", DInteger 0)]),("tail", DNull)])])) @?= hintStateTest1
      ,testCase "Hint Happy path Test2" $
        getRight(hint customState'' (DMap [("coords", DMap[("head", DMap[("col", DInteger 1),("row", DInteger 1)]),("tail", DNull)])])) @?= hintStateTest2
  ]

startStateTest1 :: Int -> Int -> State
startStateTest1 a b = State [GameDataRow [EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]1 a, ColumnInfoRow [b]]

startStateTest2 :: Int -> Int -> State
startStateTest2 a b = State [GameDataRow [EmptyCell,MarkedCell,HintCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]1 a, ColumnInfoRow [b]]

customState :: State
customState = State [GameDataRow [EmptyCell,MarkedCell,HintCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]1 0, ColumnInfoRow [0]]

hintStateTest1 :: State
hintStateTest1 = State [GameDataRow [HintCell,EmptyCell,HintCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]1 0, ColumnInfoRow [0]]

hintStateTest2 :: State
hintStateTest2 = State [GameDataRow [EmptyCell,HintCell,HintCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]1 0,
  GameDataRow [EmptyCell,HintCell,HintCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]2 0, ColumnInfoRow [0]]

customState' :: State
customState' = State [GameDataRow [EmptyCell,EmptyCell,HintCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]1 0, ColumnInfoRow [0]]

customState'' :: State
customState'' = State [GameDataRow [EmptyCell,HintCell,HintCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]1 0,
  GameDataRow [EmptyCell,EmptyCell,HintCell,
  EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell,EmptyCell]2 0, ColumnInfoRow [0]]


getLeft :: Either String b -> String
getLeft (Left a) = a
getLeft (Right _) = "Expected Left"

getRight :: Either String a -> a
getRight (Right a) = a
getRight _ = error "No Right value provided"



listOfInts :: String
listOfInts = unlines [
      "---"
    , "- 5"
    , "- 6"
  ]

listOfMaps :: String
listOfMaps = unlines [
  "---",
  "- 'row': 1",
  "  'col': 2"
  ]

mapOfList :: String
mapOfList = unlines [
  "---",
  "'coords':",
  "- 5",
  "- 6",
  "'col': 2"
  ]

mapOfMap :: String
mapOfMap = unlines [
  "---",
  "'coords':",
  "  'row': 6",
  "  'col': 5",
  "'random': 'string'"
  ]

listOfLists :: String
listOfLists =
  unlines ["---",
   "- 'hello'",
   "- 1",
   "- ",
   "  - 1",
   "  - 'level'",
   "  - 'deeper'"]


listOfListsOfLists :: String
listOfListsOfLists =
  unlines ["---",
  "- 'hello'",
  "- 1",
  "- ",
  "  - 1",
  "  - ",
  "    - 'evendeeper'",
  "    - 5",
  "  - 'deeper'"]

listOfEmptyLists :: String
listOfEmptyLists = unlines [
  "---",
  "- []",
  "- []",
  "- []",
  "- ",
  "  - []",
  "- ",
  "  - []",
  "  - []",
  "  - []"]


trickyCaseDocument :: Document
trickyCaseDocument =
 DMap [
    ("key1", DMap [
        ("key2", DList [
            DInteger 1,
            DMap [
                ("key3", DList [
                    DInteger 1,
                    DInteger 3,
                    DNull,
                    DMap [("", DNull)],
                    DMap []
                ]),
                ("key4", DString "")],
            DNull
        ])
    ]),
    ("key5", DList [])
 ]


trickyCaseString :: String
trickyCaseString = unlines [
     "---",
     "'key1':",
     "  'key2':",
     "  - 1",
     "  - 'key3':",
     "    - 1",
     "    - 3",
     "    - null",
     "    - \"\": null",
     "    - {}",
     "    'key4': \"\"",
     "  - null",
     "'key5': []"
 ]
