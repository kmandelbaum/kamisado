module KamisadoAI where

import Data.Maybe  
import Kamisado
import AI
import qualified Data.Bimap as BM
import Control.Lens
import Data.List
import Data.Ord
import Control.Exception( assert )
import Utility

allMoves' :: GameState -> [ (Move, GameState) ]
allMoves' g@(GameState posMap curPlayer colorToMove won) 
  | isJust won = []
  | isJust colorToMove = allMovesWithColor g (fromJust colorToMove)
  | otherwise = concatMap (allMovesWithColor g) (enumFrom minBound)
  where
    allMovesWithColor gs@(GameState posMap curPlayer _ _) col = result
      where
        initPos = posMap BM.!> (curPlayer, col)
        goDir d = takeWhile isFreeCell $ tail $ iterate (+d) initPos
        isFreeCell c@(x,y) = x >= 0 && x < fieldSize && 
          y >= 0 && y < fieldSize && isNothing (BM.lookup c posMap)
        dy = if curPlayer == Black then 1 else (-1)
        possibleTargets = goDir (1, dy) ++ goDir (0, dy) ++ goDir (-1, dy)
        possibleMoves = map (Move initPos) possibleTargets
        resultingStates = map (fst . acceptPlayerDecision gs . MakeMove) possibleMoves
        result = zip possibleMoves resultingStates

winScore :: Int
winScore = 1000

evaluate :: GameState -> Int
evaluate (GameState posMap _ _ won) 
  | isJust won = if (fromJust won == Black) then winScore else (-winScore)
  | otherwise = sum (map score $ wizzards Black) + sum (map score $ wizzards White)
  where
    wizzards p = zip (repeat p) (enumFrom minBound)
    score w = unCoord $ snd $ (posMap BM.!> w)

bestMove :: GameState -> Move
bestMove g = fst $ (if' (isMax g) maximumBy minimumBy) (comparing snd) scoredMoves
  where
    nextPosFun = map snd . allMoves'
    possibleMoves = allMoves' g
    scoredMoves = map (over _2 (\g -> alphabeta isMax g nextPosFun evaluate 1000)) possibleMoves

isMax :: GameState -> Bool
isMax (GameState _ p _ _) = p == Black
