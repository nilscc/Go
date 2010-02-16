{-# LANGUAGE ScopedTypeVariables #-}
module Main () where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Maybe (fromMaybe)
import Graphics.Vty
import System.Environment (getArgs)
import System.IO
import Network
import qualified Control.Exception as E

import Control
import Go
import UI
import Net

defaultPort = 5342
defaultSize = 9 -- (9,9)

showUsage = putStrLn "Go! Usage: client [IP [PORT]] | server [PORT]"

main = do

    -- todo: better argument handling
    args <- getArgs
    case args of
         ["client",ip]       -> startClient (Just ip) Nothing
         ["client",ip,port]  -> startClient (Just ip) (Just . fromIntegral $ read port)
         ["server"]          -> startHost defaultSize Nothing
         ["server",port]     -> startHost defaultSize (Just . fromIntegral $ read port)
         []                  -> commandLine
         _                   -> showUsage


-- | Configure from command line
commandLine :: IO ()
commandLine = do
    let prompt s = putStr s >> hFlush stdout >> getLine
        opt s = if null s then Nothing else Just s

    ip <- prompt "Server adress [start new server]: "
    port <- prompt "Port [5342]: "

    if null ip
       then do
           size <- prompt "Size [9]: "
           startHost (fromMaybe defaultSize $ read <$> opt size) (fromIntegral . read <$> opt port)

       else do
           startClient (opt ip) (fromIntegral . read <$> opt port)


-- | Start server
startHost :: Int -> Maybe PortNumber -> IO ()
startHost size port = do

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
                  Ok Nothing -> do
                      putStrLn "< OK"
                      startControling True read tell size
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

             tell $ Ok Nothing
             putStrLn $ "> OK"
             startControling False read tell s

         _ -> do
             putStrLn "< ?"
             tell Bye
             putStrLn "> BYE"
