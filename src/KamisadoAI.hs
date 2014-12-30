module KamisadoAI where

import Data.Maybe  
import Kamisado
import AI
import qualified Data.Bimap as BM
import Control.Lens
import Data.List
import Data.Ord
import Data.Bits
import Control.Exception( assert )
import Utility
import qualified Data.Set as S
import qualified Data.Map as M

allMoves' :: GameState -> [ (Move, GameState) ]
allMoves' g@(GameState posMap curPlayer colorToMove won _) 
  | isJust won = []
  | isJust colorToMove = allMovesWithColor g (fromJust colorToMove)
  | otherwise = concatMap (allMovesWithColor g) (enumFrom minBound)
  where
    allMovesWithColor gs@(GameState posMap curPlayer _ _ _) col = result
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
winScore = 100000

evaluate :: GameState -> Int
evaluate (GameState posMap _ _ won turnNo) 
  | isJust won = if (fromJust won == Black) then winScore - turnNo else (-winScore + turnNo)
  | otherwise = sum (map score $ wizzards Black) + sum (map score $ wizzards White)
  where
    wizzards p = zip (repeat p) (enumFrom minBound)
    score w = unCoord $ snd $ (posMap BM.!> w)

evaluate' :: GameState -> Int
evaluate' g@(GameState posMap _ _ won turnNo)
  | isJust won = if (fromJust won == Black) then winScore - turnNo else (-winScore + turnNo)
  | otherwise =   (sc Black blackSpace - sc White whiteSpace)--  (length blackSpace - length whiteSpace)
  where
    whiteSpace = allMoves' g{ _curPlayer = White, _colorToMove = Nothing }
    blackSpace = allMoves' g{ _curPlayer = Black, _colorToMove = Nothing }

    w p = S.size . S.fromList . map (\(Move p1 _) -> p1) . filter (wins p) . map fst 
    --ww = S.size . S.fromList . map (\(Move _ p2,_) -> colorMap M.! p2)
    sc p l = (w p l `shiftL` 2) + length l

    wins Black (Move _ (_,y)) = y == fieldSize - 1
    wins White (Move _ (_,y)) = y == 0

bestMove :: GameState -> (Move, Int)
bestMove g = (if' (isMax g) maximumBy minimumBy) (comparing snd) scoredMoves
  where
    nextPosFun = map snd . allMoves'
    possibleMoves = allMoves' g
    scoredMoves = map (over _2 (\g -> alphabeta isMax g nextPosFun evaluate' 5000)) possibleMoves

isMax :: GameState -> Bool
isMax (GameState _ p _ _ _) = p == Black
