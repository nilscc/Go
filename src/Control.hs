{-# LANGUAGE ScopedTypeVariables #-}

module Control
    ( startControling
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (get, modify)
import Control.Concurrent
import Data.Maybe (fromMaybe)
import Graphics.Vty
import System.IO
import System.Random

import qualified Control.Monad.State as ST
import qualified Control.Exception   as E
import qualified Data.List           as L

import Go
import Net
import UI   hiding (game, cursor)


data ControlState = ControlState
    -- networking functions
    { tellNet       :: Message -> IO ()
    , readNet       :: IO Message
    , lastId        :: Id
    -- ui functions
    , sendUI        :: UICommand -> IO ()
    , getCursor     :: IO Cursor
    , getEvent      :: IO Event
    -- game
    , game          :: Game
    , gameCommand   :: MVar GameCommand

    , myTurn        :: MVar ()
    , messages      :: Messages
    , threads       :: [ThreadId]
    }

data Messages = WaitingFor { waitingForId    :: Id
                           , controlAction   :: (Message -> Control ())
                           , waitingMessages :: Messages
                           }
              | Messages [Message]

data GameCommand = PutPlayer Player Pos (Maybe Id)

type Control a = StateT (MVar ControlState) IO a

-- state helper
get :: Control ControlState
get = ST.get >>= liftIO . readMVar
modify :: (ControlState -> ControlState) -> Control ()
modify f = ST.get >>= liftIO . (modifyMVar_ `flip` (return . f))


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

-- | Init UI and start playing!
startControling :: Bool                 -- ^ are we server?
                -> (IO Message)
                -> (Message -> IO ())
                -> Int
                -> IO ()
startControling server read tell size = do

    -- init UI
    (tId, getEvent', getCursor', sendUI') <- initUI (startGame (size,size))

    -- remember if its my turn or not (empty mvar -> not)
    myTurn'      <- newEmptyMVar
    -- send actions to the game
    gameCommand' <- newEmptyMVar

    state <- newMVar $ ControlState
        { tellNet       = tell
        , readNet       = read
        , sendUI        = sendUI'
        , getCursor     = getCursor'
        , getEvent      = getEvent'
        , game          = startGame (size,size)
        , gameCommand   = gameCommand'
        , myTurn        = myTurn'
        , messages      = Messages []
        , threads       = []
        , lastId        = 0
        }

    -- handle game commands
    forkIO . safe tell sendUI' $ evalStateT gameCommands state
    -- handle user/network input
    safe tell sendUI' $ evalStateT (handleInput server) state

-- | exception handling
safe tell sendUI = E.handle (\(_ :: E.IOException) -> do tell Bye; sendUI Shutdown)




--------------------------------------------------------------------------------
-- Sending / receiving
--------------------------------------------------------------------------------

-- | Send to "gameCommands"
sendCmd :: GameCommand -> Control ()
sendCmd cmd = do
    gc <- gameCommand <$> get
    liftIO $ putMVar gc cmd

-- | Remember whos turn it is
setMyTurn :: Bool -> Control ()
setMyTurn b = do
    mt <- myTurn <$> get
    liftIO $ if b
                then () <$ tryPutMVar mt ()
                else () <$ tryTakeMVar mt
    return ()

-- | Get a new ID
newId :: Control Id
newId = (+1) . lastId <$> get

-- | See if its my turn
isMyTurn :: Control Bool
isMyTurn = myTurn <$> get >>= liftIO . isEmptyMVar >>= return . not

-- | Handle game commands
gameCommands :: Control ()
gameCommands = forever $ do

    cmd <- runIO $ takeMVar . gameCommand <$> get

    ControlState { gameCommand = gameCommand
                 , tellNet = tellNet
                 , readNet = readNet
                 , sendUI = sendUI
                 } <- get

    let send = liftIO . tellNet
    myTurn <- isMyTurn

    case cmd of

         PutPlayer A pos Nothing
            -- our turn
            | myTurn -> do

                id <- newId
                send $ Put id pos
                waitFor id $ \msg -> do

                    ControlState { game = game } <- get
                    let (_, newgame) = playerPut A pos game
                    modify $ \s -> s { game = newgame }

                    case msg of
                         Ok (Just id) -> do liftIO . sendUI $ Redraw newgame
                                            setMyTurn False
                         No (Just id) -> return () -- fail "Invalid move!"
                         _            -> return ()

         PutPlayer B pos (Just id)
            -- our turn! player b cannot play just now!
            | myTurn    -> send $ No (Just id)
            -- ok, enemies turn:
            | otherwise -> do

                ControlState { game = game } <- get
                case playerPut B pos game of

                     (True, newgame) -> do
                         modify $ \s -> s { game = newgame }
                         liftIO . sendUI $ Redraw newgame
                         send $ Ok (Just id)
                         setMyTurn True

                     (False, _) -> send $ No (Just id)



         _ -> return ()

--------------------------------------------------------------------------------
-- User input
--------------------------------------------------------------------------------

handleInput :: Bool -> Control ()
handleInput server = do

    tellNet <- tellNet <$> get

    -- startup: decide which player begins
    when server $ do
       canIStart <- liftIO randomIO
       if canIStart
          then setMyTurn True                       -- ^ activate user input
          else liftIO $ tellNet Start               -- ^ tell other one to start

    handleNetworkInput
    handleKeyBoardInput

--
-- | Handle user (keyboard) inputs
--
handleKeyBoardInput :: Control ()
handleKeyBoardInput = do
    
    -- get our state functions
    ControlState { getEvent  = getEvent
                 , sendUI    = sendUI
                 , getCursor = getCursor
                 } <- get

    forever $ do

        key <- liftIO getEvent
        case key of

             EvResize x y           -> liftIO . sendUI $ Resize x y

             EvKey KUp    []        -> liftIO $ sendUI CursorUp
             EvKey (KASCII 'k') []  -> liftIO $ sendUI CursorUp
             EvKey KDown  []        -> liftIO $ sendUI CursorDown
             EvKey (KASCII 'j') []  -> liftIO $ sendUI CursorDown
             EvKey KLeft  []        -> liftIO $ sendUI CursorLeft
             EvKey (KASCII 'h') []  -> liftIO $ sendUI CursorLeft
             EvKey KRight []        -> liftIO $ sendUI CursorRight
             EvKey (KASCII 'l') []  -> liftIO $ sendUI CursorRight

             EvKey KEnter []        -> do

                Cursor x y <- liftIO getCursor
                sendCmd $ PutPlayer A (fromIntegral x, fromIntegral y) Nothing

             -- shutdown
             EvKey KEsc   []        -> do
                 fail "Shutdown"

             _ -> return ()


--
-- | Handle network input. This starts a seperate thread and returns the thread id
--
handleNetworkInput :: Control ()
handleNetworkInput = do

    -- handle incoming network messages
    s  <- ST.get
    ControlState { tellNet = tellNet, sendUI = sendUI } <- get

    -- start a second thread with the same state
    id <- liftIO . forkIO . safe tellNet sendUI . (evalStateT `flip` s) . forever $ do

        msg <- runIO $ readNet <$> get

        case msg of

             -- Start -> setMyTurn True
             -- Bye   -> fail "Bye received. Shutting down."
             _     -> handleWaiting msg

             -- Put id pos -> sendCmd $ PutPlayer B pos (Just id)

    -- remember the thread id
    modify $ \s -> s { threads = (threads s) ++ [id] }



-- | Add a message, handle waiting messages and run @Message -> Control ()@
-- action if waiting ID is found.
handleWaiting :: Message -> Control ()
handleWaiting msg = do

    msgs <- messages <$> get

    let

        -- Check if we got a reply to any of our waiting messages. If not then
        -- just add this message to our message list...
        getWaiting :: Messages -> (Maybe (Control ()), Messages)

        getWaiting (WaitingFor i ctrl msgs')
            -- we found our ID we were waiting for:
            | getId msg == (Just i) = (Just (ctrl msg), msgs')
            -- maybe look in the next element of our Messages queue
            | otherwise             = let (f, m) = getWaiting msgs' in (f, WaitingFor i ctrl m)

        getWaiting (Messages msgs') = (Nothing, Messages (msgs' ++ [msg])) -- run the oldest first

        getId :: Message -> Maybe Id
        getId (Ok i)    = i
        getId (No i)    = i
        getId (Put i _) = Just i
        getId _         = Nothing

        -- get our new messages
        (f, msgs') = getWaiting msgs

    -- remember new message queue
    modify $ \s -> s { messages = msgs' }
    -- run our Control function if any was found
    fromMaybe (return ()) f
    -- check if we still have waiting messages, if not run all waiting messages
    case msgs' of
         Messages m -> runMessages m
         _          -> return ()


-- | Run all messages which are or were waiting
runMessages :: [Message] -> Control ()
runMessages msgs = do
    mapM_ run msgs
    modify $ \s -> s { messages = Messages [] }

  where run :: Message -> Control ()
        run Start           = setMyTurn True
        run Bye             = fail "Bye received. Shutting down."
        run (Put id pos)    = sendCmd $ PutPlayer B pos (Just id)
        run _               = return () -- fail $ "Unknown message received: " ++ show m

--
-- | Wait for a message with our ID
--
waitFor :: Id
        -> (Message -> Control ())  -- ^ Control action to run when the ID is found
        -> Control ()
waitFor id msgToControl = modify $ \s -> s { messages = WaitingFor id msgToControl (messages s) }


--------------------------------------------------------------------------------
-- Helper
--------------------------------------------------------------------------------

runIO f = f >>= liftIO . id
