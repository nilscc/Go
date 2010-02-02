{-# LANGUAGE ScopedTypeVariables #-}
module Main () where

import Control.Monad
import Control.Concurrent
import Data.Maybe (fromMaybe)
import Graphics.Vty
import System.Environment (getArgs)
import Network
import qualified Control.Exception as E

import Go
import UI
import Net

defaultPort = 5342
size = (9,9)

main = do

    args <- getArgs
    case args of
         ["client",ip]       -> startClient (Just ip) Nothing
         ["client",ip,port]  -> startClient (Just ip) (Just . fromIntegral $ read port)
         ["server"]          -> startHost Nothing
         ["server",port]     -> startHost (Just . fromIntegral $ read port)
         _                   -> showUsage

showUsage = putStrLn "Go! Usage: client [IP [PORT]] | server [PORT]"


-- | Start server
startHost :: Maybe PortNumber -> IO ()
startHost port = do

    (read, tell) <- startServer $ fromMaybe defaultPort port

    let quit = do tell Bye; putStrLn "> BYE"

    aw <- read
    case aw of
         Hello -> do
             putStrLn "< HELLO"
             tell $ Size size
             putStrLn $ "> SIZE " ++ show size
             aw <- read
             case aw of
                  Ok -> do
                      putStrLn "< OK"
                      start' True read tell size
                  _  -> quit
         _ -> quit

-- | Start client
startClient :: Maybe HostName -> Maybe PortNumber -> IO ()
startClient (Just ip) port = do

    (read, tell) <- connect ip $ fromMaybe defaultPort port

    tell Hello
    putStrLn "> HELLO"

    aw <- read
    case aw of

         Size s -> do
             putStrLn $ "< SIZE " ++ show s

             tell Ok
             putStrLn $ "> OK"
             start' False read tell s

         _ -> do
             putStrLn "< ?"
             tell Bye
             putStrLn "> BYE"


-- | Init UI and start playing!
start' :: Bool                  -- our turn?
       -> (IO Message)
       -> (Message -> IO ())
       -> (Int, Int)
       -> IO ()
start' begin read tell size = do

    vty <- mkVty

    -- init UI
    (tId, uiState, sendUI) <- initUI size vty

    let safe = E.handle (\(_ :: E.IOException) -> do tell Bye; shutdown vty)

    forkIO . safe . forever $ do
        msg <- read
        case msg of
             Net.Put p -> sendUI $ UI.Put B p
             _ -> return ()

    safe .  forever $ do

        key <- next_event vty
        case key of

             EvResize x y           -> sendUI $ Resize x y

             EvKey KUp    []        -> sendUI CursorUp
             EvKey KDown  []        -> sendUI CursorDown
             EvKey KLeft  []        -> sendUI CursorLeft
             EvKey KRight []        -> sendUI CursorRight

             EvKey KEnter []        -> do

                 UIState { cursor = Cursor x y } <- uiState
                 let pos = (fromIntegral x, fromIntegral y)
                 tell $ Net.Put pos
                 sendUI $ UI.Put A pos

             -- shutdown
             EvKey KEsc   []        -> killThread tId >> fail "Shutdown"

             _ -> sendUI Redraw
