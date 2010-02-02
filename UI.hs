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
import Control.Monad.State
import qualified Control.Exception as E
import qualified Data.Map          as M
import qualified Data.Maybe        as Maybe
import qualified Data.List         as L

import Graphics.Vty

import Go

data UIState = UIState
    { vty           :: Vty
    , game          :: Game
    , uiCommand     :: MVar UICommand
    , cursor        :: Cursor
    , query         :: MVar UIState
    }


type UI a = StateT UIState IO a

data UICommand = Put Player Pos
               | PutAtCurrentPos Player
               -- movement
               | CursorUp
               | CursorDown
               | CursorLeft
               | CursorRight
               -- drawing the window
               | Redraw
               | Resize Int Int
               -- shutdown
               | Shutdown
               -- Queries
               | AskState

data Direction = DUp | DDown | DLeft | DRight



--------------------------------------------------------------------------------
-- Start listening for UI events
--------------------------------------------------------------------------------

-- | Initiate and run our UI state
initUI :: (Int, Int)   -- ^ size of the game
       -> Vty          -- ^ Vty output
       -> IO (ThreadId, IO UIState, UICommand -> IO ())
initUI (h,w) vty = do

    cmd <- newMVar Redraw
    qry <- newEmptyMVar

    let game = startGame (w,h)
        curs = Cursor 1 1
        ask  = do putMVar cmd AskState; takeMVar qry

    -- start handling commands
    tId <- forkIO $ () <$ evalStateT handleCommands (UIState vty game cmd curs qry) `E.catch` \(_ :: E.IOException) -> return ()

    return (tId, ask, putMVar cmd)

-- | Handle incoming commands from other threads
handleCommands :: UI ()
handleCommands = forever $ do

    UIState { uiCommand = mvar, query = qry, cursor = (Cursor cX cY) } <- get
    cmd <- liftIO $ takeMVar mvar

    case cmd of

         Redraw             -> drawGame
         Resize x y         -> resize (x,y)

         Put p pos          -> putGame p pos
         PutAtCurrentPos p  -> putGame p (fromIntegral cX, fromIntegral cY)

         CursorUp           -> moveCursor DUp
         CursorDown         -> moveCursor DDown
         CursorLeft         -> moveCursor DLeft
         CursorRight        -> moveCursor DRight

         Shutdown           -> fail "handleCommands: Shutdown"

         AskState           -> get >>= liftIO . putMVar qry


--------------------------------------------------------------------------------
-- User controls
--------------------------------------------------------------------------------

-- | Put a new stone to the game
putGame :: Player -> Pos -> UI ()
putGame p pos = do

    -- update our game
    modify $ \s@(UIState { game = game }) ->
        let (valid,newGame) = playerPut p pos game
        in s { game = if valid
                         then newGame
                         else game
             }

    -- draw it
    drawGame

-- | Move the cursor
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


        in  s { cursor = Cursor x' y' }

    drawGame




--------------------------------------------------------------------------------
-- Drawing stuff
--------------------------------------------------------------------------------

-- | Draw game, call resize with current terminal width/height
drawGame :: UI ()
drawGame = do

    UIState { vty = vty, game = game } <- get

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
