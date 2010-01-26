{-# LANGUAGE ScopedTypeVariables #-}

-- module UI
    -- (
    -- ) where

import Control.Applicative
import Control.Concurrent
import qualified Control.Exception as E
import Control.Monad.State

import qualified Data.Map   as M
import qualified Data.Maybe as Maybe

import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper

import Go

data UIState = UIState
    { window        :: Window
    , game          :: Game
    , uiCommand     :: MVar UICommand
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
               | Resize
               -- shutdown
               -- | Shutdown

main = do
    start

    (tId, sendUI) <- initUI (9,9)

    E.handle (\(e :: E.IOException) -> {- sendUI Shutdown >> -} end) . forever $ do

    key <- getCh
    case key of
         KeyResize      -> sendUI Resize

         KeyUp          -> sendUI CursorUp
         KeyDown        -> sendUI CursorDown
         KeyLeft        -> sendUI CursorLeft
         KeyRight       -> sendUI CursorRight

         KeyChar '1'    -> sendUI $ PutAtCurrentPos A
         KeyChar '2'    -> sendUI $ PutAtCurrentPos B

         KeyF 1         -> killThread tId >> fail "Shutdown"

         _ -> return ()


-- | Initiate and run our UI state
initUI :: (Int, Int)   -- ^ size of the game
       -> IO (ThreadId, UICommand -> IO ())
initUI (h,w) = do

    let w'  = w*2 + 3
        h'  = h   + 3

    win <- newWin h' w' 0 0
    wMove win 1 2
    cmd <- newMVar Resize

    let game = startGame (w,h)

    id <- forkIO $ () <$ evalStateT handleCommands (UIState win game cmd) -- `E.catch` \(e :: E.IOException) -> return ()

    return (id, putMVar cmd)

-- | Handle incoming commands from other threads
handleCommands :: UI ()
handleCommands = forever $ do

    UIState { uiCommand = mvar } <- get
    cmd <- liftIO $ takeMVar mvar

    let putGame p pos = do
        modify $ \s@(UIState { game = game }) ->
            let (valid,newGame) = playerPut p pos game
            in s { game = if valid
                             then newGame
                             else game
                 }
        drawGame

    case cmd of
         Redraw      -> drawGame
         Resize      -> resize

         Put p pos         -> putGame p pos
         PutAtCurrentPos p -> getCurrentPos >>= putGame p

         CursorUp    -> moveCursor (subtract 1) id
         CursorDown  -> moveCursor (+1)         id
         CursorLeft  -> moveCursor id           (subtract 2)
         CursorRight -> moveCursor id           (+2)

         -- Shutdown    -> fail "Shutting down ncurses."

resize :: UI ()
resize = do
    UIState { window = win } <- get

    -- liftIO $ do
        -- endWin
        -- resetParams

    (y, x) <- liftIO $ resizeui
    (h, w) <- liftIO $ getMaxYX win

    let
        y' = (y - h) `quot` 2
        x' = (x - w) `quot` 2

    liftIO $ do
        mvWin win y' x'
        wclear stdScr
        refresh

    drawGame


-- | Current position of the cursor. Note: Whereas the cursor position in
-- ncurses is usually (y,x) this will return the actual (x,y) position as used
-- in the Go module.
getCurrentPos :: UI Pos
getCurrentPos = do
    UIState { window = win } <- get

    (curY, curX) <- liftIO $ getYX win

    let y = curY
        x = (curX `quot` 2)

    return $! (x,y)

-- | Move our cursor
moveCursor :: (Int -> Int) -- ^ row modifier
           -> (Int -> Int) -- ^ col modifier
           -> UI ()
moveCursor yF xF = do
    UIState { window = win } <- get

    (curY, curX) <- liftIO $ getYX win
    (maxY, maxX) <- liftIO $ getMaxYX win

    -- schwachsinn... funktioniert noch nich
    case (yF curY, xF curX) of
         (y,x) | 0 < y && y < (maxY - 2) && 1 < x && x < (maxX - 2) -> do
             liftIO $ wMove win y x
             drawGame
         _ -> return ()


-- | Draw the current game
drawGame :: UI ()
drawGame = do

    UIState { window = win, game = game } <- get

    let ((sizeX,sizeY),_) = M.findMax (gameMap game)

    -- remember cursor position
    (y,x) <- liftIO $ getYX win

    liftIO $ do
        -- upper border
        mvWAddStr win 0 0 $ [ulCorner]
                         ++ take (sizeX*2 +1) (cycle [hLine])
                         ++ [urCorner]
        -- scores
        mvWAddStr win 0 2 $ "X:" ++ show (scoreA game)
        let scB = "O:" ++ show (scoreB game)
        mvWAddStr win 0 (sizeX*2+1 - length scB) scB

    -- game field
    mapM_ drawRow [1..sizeY]

    liftIO $ do
        -- lower border
        mvWAddStr win (sizeY+1) 0 $ [llCorner] ++ (take (sizeX*2 +1) $ cycle [hLine]) ++ [lrCorner]

        -- reset cursor
        wMove win y x
        wRefresh win


-- | Draw one single row
drawRow :: Int -> UI ()
drawRow n = do

    UIState { window = win, game = game } <- get

    let
        row = M.elems $ M.filterWithKey (\(_,y) _ -> y == n) (gameMap game)
        style st f = do
            style <- head <$> convertStyles [st]
            wWithStyle win style f

        drawField (Just A) = wAddStr win " " >> style (Style GreenF DefaultB) (wAddStr win "×")
        drawField (Just B) = wAddStr win " " >> style (Style RedF   DefaultB) (wAddStr win "O")
        drawField Nothing  = wAddStr win " ·"

    liftIO $ do
        mvWAddStr win n 0 [vLine]
        mapM_ drawField row
        mvWAddStr win n (length row * 2 +1) [' ', vLine]

