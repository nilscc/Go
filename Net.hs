{-# LANGUAGE ScopedTypeVariables #-}

module Net
    ( connect
    , startServer
    -- * Data definitions
    , Message (..)
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe (fromMaybe)
import System.IO
import Network
import qualified Control.Exception as E

import Net.Parser

type Pos = (Int,Int)

-- | Connect to a server, start listening for incoming messages.
-- 
-- This returns to functions. The first will read all incoming messages, the
-- second will handle outgoing messages. Example:
--
-- > do
-- >    (read, tell) <- connect ...
-- >    msg <- read
-- >    tell Hello
--
connect :: HostName
        -> PortNumber
        -> IO (IO Message, Message -> IO ())
connect host port = start =<< connectTo host (PortNumber port)

-- | Start a server, wait for incoming connections. See "connect" for more
-- informations...
startServer :: PortNumber -> IO (IO Message, Message -> IO ())
startServer port = do

    socket <- listenOn (PortNumber port)
    (h, hostname, port') <- accept socket
    hSetBuffering h LineBuffering

    putStrLn $ "New client from: " ++ hostname ++ ":" ++ show port'

    start h

-- | Start everything...
start :: Handle -> IO (IO Message, Message -> IO ())
start h = do

    hSetBuffering h LineBuffering

    tell    <- newEmptyMVar
    read    <- newChan
    threads <- newEmptyMVar
    what    <- newEmptyMVar

    let safe = E.handle $ \(_ :: E.IOException) -> do
        ids <- fromMaybe [] <$> tryTakeMVar threads
        mapM_ killThread ids

    lId <- forkIO . safe . forever $ listenForIncoming h (writeChan read) (putMVar tell) what
    cId <- forkIO . safe . forever $ handleCommands    h (takeMVar tell)

    putMVar threads [lId, cId]

    return (readChan read, putMVar tell)

-- | Listen on handle
listenForIncoming :: Handle                 -- ^ handle to listen on
                  -> (Message -> IO ())     -- ^ reply function
                  -> (Message -> IO ())    -- ^ tell  function
                  -> MVar ()                -- ^ mvar whether we're waiting for a "WHAT"
                  -> IO ()
listenForIncoming handle reply tell what = do

    l <- hGetLine handle

    case decode l of

         Bye  -> do
             hClose handle
             fail "listenForIncoming: Connection closed by remote side"

         What -> do
             empty <- isEmptyMVar what
             when empty $ do
                tell What -- send "what?!" reply
                putMVar what ()  -- remember what reply

         msg  -> do
             tryTakeMVar what -- "reset" what
             reply msg        -- everything ok here


-- | Handle incoming events
handleCommands :: Handle -> (IO Message) -> IO ()
handleCommands handle command = do

    cmd <- command

    case cmd of

         Bye   -> do
             hPutStrLn handle (encode Bye)
             hClose handle
             fail "handleCommands: Connection closed"

         msg   -> hPutStrLn handle (encode msg)
