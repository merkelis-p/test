{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.State.Strict
    ( evalStateT, get, modify, put, StateT )
import Data.ByteString as B ( empty, ByteString )
import Data.Either as E (fromRight)
import qualified Data.List as L
import Data.Text as T ( concat, drop, pack, unpack, Text )
import Data.Text.Encoding.Base64 (decodeBase64)
import Data.Text.IO as TIO ( hPutStrLn, putStrLn )
import Data.List.Split as S ( splitOn )
import Data.Char (isSpace)
import Lib1 ( emptyState, mkCheck, render, toggle, State )
import Lib2 ( renderDocument )
import Lib4 ( parseDocument, hint, gameStart, toggle, Hint, GameStart)
import Types(Check (Check), toDocument, fromDocument, Coord (Coord))
import Network.Wreq
    ( post, postWith, defaults, header, responseBody )
import qualified Network.Wreq as Wreq

import Control.Lens
import System.Console.Repline
  ( CompleterStyle (Word),
    ExitDecision (Exit),
    HaskelineT,
    WordCompleter,
    evalRepl,
  )
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr)

import Data.String.Conversions
import qualified Data.ByteString as Preulde
import Data.Aeson (ToJSON(toJSON))

type Repl a = HaskelineT (StateT (String, Lib1.State) IO) a

commandShow :: String
commandShow = "show"

commandCheck :: String
commandCheck = "check"

commandToggle :: String
commandToggle = "toggle"

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd c
  | trim c == commandShow = Main.show >>= liftIO . Prelude.putStrLn 
  | trim c == commandCheck = check >>= liftIO . Prelude.putStrLn
  | commandToggle `L.isPrefixOf` trim c = do
    case tokens c of
      [_] -> liftIO $ Prelude.putStrLn $ "Illegal format, \"" ++ commandToggle ++ " expects at least one argument"
      t -> Main.toggle (L.drop 1 t)
cmd c = liftIO $ Prelude.putStrLn $ "Unknown command: " ++ c

tokens :: String -> [String]
tokens s = L.filter (not . Prelude.null) $ S.splitOn " " s

trim :: String -> String
trim = f . f
  where f = L.reverse . L.dropWhile isSpace

toggle :: [String] -> Repl ()
toggle [r, c] = do
  (url,_) <- lift get
  let opts = defaults & header "Content-type" .~ ["text/x-yaml"]
  let body = cs $ renderDocument $ toDocument $ Check [Coord ((read c::Int)-1) ((read r::Int)-1)] :: ByteString
  _ <- liftIO $ postWith opts (url ++ "/update") body
  return ()
toggle _ = liftIO $ Preulde.putStrLn $ "Unsupported format"

check :: Repl String
check = do
  (url, _) <- lift get
  resp <- liftIO $ Wreq.get (url ++ "/check")
  pure $ cs $ resp ^. responseBody

show :: Repl String
show = do
  (url,s) <- lift get
  r <- liftIO $ Wreq.get $ url ++ "/show"
  let coords = Lib4.parseDocument (cs (r ^. responseBody))
  case coords of
    Left s -> pure $ "[Error parsing server message] " ++ s
    Right r -> case Lib4.toggle s r of
      Left s -> pure $ "[Error on Client side] " ++ s
      Right r -> pure $ Lib1.render r

-- Tab Completion: return a completion for partial words entered
completer :: Monad m => WordCompleter m
completer n = do
  let names = [commandShow, commandCheck, commandToggle]
  return $ Prelude.filter (L.isPrefixOf n) names

ini :: Repl ()
ini = do
  (url, s) <- lift get
  r <- liftIO $ Wreq.get url
  let gs = Lib4.parseDocument (cs (r ^. responseBody)) >>= fromDocument
  case (gs :: Either String Lib4.GameStart) of
    Left msg -> liftIO $ fatal $ cs msg
    Right d -> do
      lift $ put (url, Lib4.gameStart s d)
      liftIO $ TIO.putStrLn "Welcome to Bimaru v4. Press [TAB] for available commands list"

fatal :: Text -> IO ()
fatal msg = do
  TIO.hPutStrLn stderr $ T.concat ["ERROR: ", msg]
  exitFailure

final :: Repl ExitDecision
final = do
  liftIO $ TIO.putStrLn "Goodbye!"
  return Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [token] -> run $ T.pack token
    _ -> fatal "token not provided, expected at least one command argument"

run :: T.Text -> IO ()
run token = do
  -- Dear students, it is not against you, it is against silly source code crawlers on the Internet
  -- let url = E.fromRight (error "Cannot decode url") $ decodeBase64 $ T.drop 6 "f6675cYmltYXJ1LmhvbWVkaXIuZXU="
  let fullUrl = T.unpack (T.concat ["http://", "localhost:8080", "/game/", token])
  evalStateT (evalRepl (const $ pure ">>> ") cmd [] Nothing Nothing (Word completer) ini final) (fullUrl, Lib1.emptyState)
