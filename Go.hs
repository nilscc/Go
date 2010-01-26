{-# LANGUAGE BangPatterns #-}

module Go
    (
    -- * Data definitions
      Player (..)
    , Game (..)
    , Pos
    
    -- * Play the game
    , startGame
    , playerPut
    ) where
import Control.Applicative
import Control.Monad.State

import qualified Data.List  as L
import qualified Data.Map   as M
import qualified Data.Maybe as Maybe

data Player = A | B deriving (Eq, Show)
data Game = Game
    { gameMap   :: M.Map Pos (Maybe Player)
    , scoreA    :: Int
    , scoreB    :: Int
    }
type Pos = (Int, Int)


-- | Generate our game map
startGame :: (Int, Int) -- ^ size: (width, height)
          -> Game
startGame (x,y) = Game
    { gameMap = M.fromList [ ((x', y'), Nothing) | x' <- [1..x], y' <- [1..y] ]
    , scoreA  = 0
    , scoreB  = 0
    }

-- | Put a new stone on the field
playerPut :: Player         -- ^ Current player
          -> Pos            -- ^ Position
          -> Game           -- ^ Current game
          -> (Bool, Game)   -- ^ (True, newgame) if valid move, (False, oldgame) if invalid move
playerPut player pos game = do

    case M.lookup pos (gameMap game) of
         Just Nothing | not (isDead pos newGame) || or (map (isDead `flip` newGame) (hostileSurrounding player pos game)) ->
             (True, foldr (\pos' game' -> removeDead player pos' game') newGame $ surrounding pos)
         _ -> (False, game)

  where newGame = game { gameMap = M.alter (const $ Just (Just player)) pos (gameMap game) }

-- All surrounding stones of a current position
surrounding :: Pos -> [Pos]
surrounding (x,y) = [ (x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1], abs(dx + dy) == 1 ]

-- All hostile surrounding stones
hostileSurrounding :: Player -> Pos -> Game -> [Pos]
hostileSurrounding player pos Game { gameMap = game } =

    M.keys $ M.filterWithKey filterHostile game

  where filterHostile pos' player' = pos' `elem` surrounding pos && isHostile player'
        isHostile p                = Maybe.isJust p && p /= Just player

-- get all "connected" stones
connected :: Player -> Pos -> Game -> [Pos]
connected player pos Game { gameMap = game } = con [] pos

  where con !found pos
            | M.lookup pos game == Just (Just player) = 
                flip (foldr `flip` found) (pos : surrounding pos) $ \pos' found' ->
                    case M.lookup pos' game of
                         -- new stone of our player found
                         Just (Just p) | p == player && not (pos' `elem` found') -> con (pos' : found') pos'
                         -- nothing new
                         _ -> found'

            | otherwise = found

-- check if a stone is dead
isDead :: Pos -> Game ->  Bool
isDead pos game =
    case M.lookup pos (gameMap game) of
         Just (Just p) -> let getFree pos' rest = case M.lookup pos' (gameMap game) of Just Nothing -> pos' : rest; _ -> rest
                          in  null . concat . map (foldr getFree [] . surrounding) $ connected p pos game
         _ -> False

-- remove all dead stones of the enemy player, return the number of killed stones and 
removeDead :: Player    -- ^ Current player
           -> Pos       -- ^ Position
           -> Game      -- ^ Current game
           -> Game
removeDead player pos game =
    case M.lookup pos (gameMap game) of
         Just (Just p) | p /= player && isDead pos game ->
             foldr (\pos' game' -> game' { gameMap = M.alter (const $ Just Nothing) pos' (gameMap game')
                                         , scoreA  = scoreA game' + if player == A then 1 else 0
                                         , scoreB  = scoreB game' + if player == B then 1 else 0
                                         }) game $ connected p pos game
         _ -> game
