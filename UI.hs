{-# LANGUAGE ScopedTypeVariables #-}

module UI
    (
    -- * Initializing
      initUI
    -- * Data definitions
    , UICommand (..)
    , UIState (..)
    ) where

import Control.Applicative hiding ((<|>))
import Control.Concurrent
import Control.Monad.State hiding (get, modify)

import qualified Control.Monad.State as ST
import qualified Control.Exception   as E
import qualified Data.Map            as M
import qualified Data.Maybe          as Maybe
import qualified Data.List           as L

import Graphics.Vty

import Go


data UIState = UIState
    { vty           :: Vty
    , game          :: Game
    , uiCommand     :: MVar UICommand
    , cursor        :: Cursor
    }

type UI a = StateT (MVar UIState) IO a

data UICommand
    -- movement
    = CursorUp
    | CursorDown
    | CursorLeft
    | CursorRight
    -- drawing the window
    | Redraw Game
    | Resize Int Int
    -- shutdown
    | Shutdown

data Direction = DUp | DDown | DLeft | DRight

-- | mvar state access
get :: UI UIState
get = ST.get >>= liftIO . readMVar
modify :: (UIState -> UIState) -> UI ()
modify f = ST.get >>= liftIO . (modifyMVar_ `flip` (return . f))

--------------------------------------------------------------------------------
-- Start listening for UI events
--------------------------------------------------------------------------------

-- | Initiate and run our UI state
initUI :: Game                          -- ^ initial game
       -> IO ( ThreadId                 -- ^ UI thread
             , IO Event                 -- ^ Vty events
             , IO Cursor                -- ^ current cursor position
             , UICommand -> IO ()
             )
initUI game = do

    let curs      = Cursor 1 1

    vty   <- mkVty
    cmd   <- newMVar $ Redraw game
    state <- newMVar $ UIState vty game cmd curs

    -- start handling commands
    tId <- forkIO $ () <$ evalStateT handleCommands state `E.catch` \(_ :: E.IOException) -> return ()

    let askCursor = cursor <$> readMVar state

    return (tId, next_event vty, askCursor, putMVar cmd)

-- | Handle incoming commands from other threads
handleCommands :: UI ()
handleCommands = forever $ do

    UIState { uiCommand = mvar, cursor = (Cursor cX cY) } <- get
    cmd <- liftIO $ takeMVar mvar

    case cmd of

         Redraw game        -> drawGame $ Just game
         Resize x y         -> resize (x,y)

         -- Put p pos          -> putGame p pos
         -- PutAtCurrentPos p  -> putGame p (fromIntegral cX, fromIntegral cY)

         CursorUp           -> moveCursor DUp
         CursorDown         -> moveCursor DDown
         CursorLeft         -> moveCursor DLeft
         CursorRight        -> moveCursor DRight

         Shutdown           -> fail "handleCommands: Shutdown"


--------------------------------------------------------------------------------
-- User controls
--------------------------------------------------------------------------------

-- | Move the cursor
moveCursor :: Direction -> UI ()
moveCursor dir = do

    let (fX,fY) = case dir of
                         DUp     -> (id,            subtract 1)
                         DDown   -> (id,            (+1))
                         DLeft   -> (subtract 1,    id)
                         DRight  -> ((+1),          id)

    modify $ \s@(UIState { cursor = (Cursor x y), game = game }) ->
        let

            ((gameX,gameY),_) = M.findMax (gameMap game)
            (x',y') = case (fX x, fY y) of
                           (xx,yy) | 1 <= xx && xx <= (fromIntegral gameX) && 1 <= yy && yy <= (fromIntegral gameY) -> (xx,yy)
                           _ -> (x,y)

        in s { cursor = Cursor x' y' }

    drawGame Nothing




--------------------------------------------------------------------------------
-- Drawing stuff
--------------------------------------------------------------------------------

-- | Draw game, call resize with current terminal width/height
drawGame :: Maybe Game -> UI ()
drawGame game = do

    case game of
         Just g -> modify $ \s -> s { game = g }
         _ -> return ()

    UIState { vty = vty } <- get

    -- get current screen size
    (x,y) <- liftM2 (,) region_width region_height <$> display_bounds (terminal vty)

    resize (fromIntegral x, fromIntegral y)

-- | Resize & redraw the current game
resize :: (Int, Int) -- ^ display bounds (from EvResize)
       -> UI ()
resize (x,y) = do

    UIState { vty = vty, game = game, cursor = (Cursor cX cY) } <- get

    let gameIm = gameImage game
        gameX  = fromIntegral $ image_width  gameIm
        gameY  = fromIntegral $ image_height gameIm

        y'      = (y - gameY) `quot` 2
        x'      = (x - gameX) `quot` 2

        top     = char_fill def_attr ' ' 1 y'
        left    = char_fill def_attr ' ' x' 1

        cursX   = x' + (fromIntegral cX)*2
        cursY   = y' + fromIntegral cY
        cursor  = if 0 < cursX && cursX < x && 0 < cursY && cursY < y
                     then Cursor (fromIntegral cursX) (fromIntegral cursY)
                     else NoCursor

        picture = Picture cursor (top <-> (left <|> gameIm)) (Background ' ' def_attr)

    liftIO $ update vty picture

-- | Image for our game
gameImage :: Game
          -> Image
gameImage game = 
    let

        ((x,y),_)   = M.findMax (gameMap game)

        scoreA'     = [vertic] ++ "A:" ++ show (scoreA game)
        scoreB'     = "B:" ++ show (scoreB game) ++ [vertic]

        topRow      = string def_attr $ [upperL] ++ scoreA' ++ replicate (x * 2 + 1 - length scoreA' - length scoreB') vertic ++ scoreB' ++ [upperR]
        botRow      = string def_attr $ [lowerL] ++ replicate (x * 2 + 1) vertic ++ [lowerR]

        drawField (Just A) = string (def_attr `with_fore_color` bright_green) " O"
        drawField (Just B) = string (def_attr `with_fore_color` bright_red  ) " X"
        drawField Nothing  = string (def_attr `with_fore_color` bright_black) " ·"

        row n       = string def_attr [horiz] <|> (horiz_cat . map drawField . M.elems $ M.filterWithKey (\(_,y) _ -> y == n) (gameMap game)) <|> string def_attr [' ',horiz]

    in topRow <-> (vert_cat $ map row [1..y]) <-> botRow

-- | Border stuff...
upperL = '┌'
upperR = '┐'
lowerL = '└'
lowerR = '┘'
vertic = '─'
horiz  = '│'
