{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Web.Spock
import Web.Spock.Config

import Data.Yaml as Y
import LibServer (calcOccupied, Coord(..), GameState(..), isResultCorrect, updateGameState)
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson as A
import Data.HashMap.Strict as HMS
import Data.UUID.V4 (nextRandom)
import Data.UUID (toText)
import Control.Monad.Cont (MonadIO(liftIO))
import GHC.Conc (TVar, readTVarIO, STM, readTVar, writeTVar, retry, atomically, newTVarIO)
import Types (Check(Check))

data ServerState where
  BimaruState :: TVar (HashMap String GameState) -> ServerState

main :: IO ()
main =
    do ref <- newTVarIO empty
       spockCfg <- defaultSpockCfg () PCNoDatabase (BimaruState ref)
       runSpock 8080 (spock spockCfg app)

app :: SpockM () () ServerState ()
app = do
    get root $ do
        uuid <- liftIO nextRandom
        text $ toText uuid

    get ("game" <//> var) $ \(uuid :: String) -> do
        (BimaruState _state) <- getState
        state <- liftIO $ readTVarIO _state
        if not $ member uuid state
            then do
                liftIO $ atomically $ modifyServerState (BimaruState _state) (const True) (insert uuid (GameState []))
                text $ decodeUtf8 $ Y.encode $ toJSON $ object ["occupied_rows" A..= calcOccupied row secret, "occupied_cols" A..= calcOccupied col secret, "uuid" A..= uuid]
        else text $ decodeUtf8 $ Y.encode $ toJSON $ object ["occupied_rows" A..= calcOccupied row secret, "occupied_cols" A..= calcOccupied col secret, "uuid" A..= uuid]


    get ("game" <//> var <//> "show") $ \(uuid :: String) -> do
        (BimaruState _state) <- getState
        state <- liftIO $ readTVarIO _state
        text $ decodeUtf8 $ Y.encode $ toJSON $ HMS.lookup uuid state >>= \(GameState coords) -> pure coords


    post ("game" <//> var <//> "update") $ \(uuid :: String) -> do
        coords <- Y.decode <$> body
        _state <- getState
        _ <- liftIO $ atomically $ modifyServerState _state (member uuid) (\mp ->
            let current = HMS.lookup uuid mp in
            update (const (updateGameState <$> current <*> coords)) uuid mp)
        text "Ok"


    get ("game" <//> var <//> "check") $ \(uuid :: String) -> do
        (BimaruState _state) <- getState
        state <- liftIO $ readTVarIO _state
        if member uuid state
            then if isResultCorrect (HMS.lookupDefault (GameState []) uuid state) (GameState secret)
                then text "Well done!"
                else text "Try Harder!"
        else do
            text "Start playing first!"

secret :: [Coord]
secret = [
    Coord {row = 2, col = 1},
    Coord {row = 3, col = 1},
    Coord {row = 4, col = 1},
    Coord {row = 5, col = 1},
    Coord {row = 3, col = 3},
    Coord {row = 4, col = 3},
    Coord {row = 5, col = 3},
    Coord {row = 0, col = 5},
    Coord {row = 1, col = 5},
    Coord {row = 2, col = 5},
    Coord {row = 7, col = 0},
    Coord {row = 7, col = 3},
    Coord {row = 9, col = 3},
    Coord {row = 9, col = 0},
    Coord {row = 9, col = 8},
    Coord {row = 9, col = 9},
    Coord {row = 6, col = 8},
    Coord {row = 6, col = 9},
    Coord {row = 2, col = 8},
    Coord {row = 2, col = 9}
    ]

--- STM
modifyServerState :: ServerState -> (HashMap String GameState -> Bool) -> (HashMap String GameState -> HashMap String GameState) -> STM ()
modifyServerState (BimaruState state) checkFunc fn = do
    curr <- readTVar state
    if not (checkFunc curr)
        then retry
    else  do
        let newState = fn curr
        writeTVar state newState