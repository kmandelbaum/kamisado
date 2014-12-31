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
import Data.Function.Memoize
import qualified Debug.Trace as DT
import Control.Parallel.Strategies
import Control.DeepSeq

instance NFData Coord where
  rnf (Coord x) = rnf x `seq` ()

instance NFData Move where
  rnf (Move p1 p2) = rnf p1 `seq` rnf p2 `seq` ()

instance Memoizable PlayerId where
  memoize f = \p -> case p of 
              Black -> blackValue
              White -> whiteValue
    where (blackValue, whiteValue) = (f Black, f White)

{-# INLINE allMoves' #-}
allMoves' :: GameState -> [ (Move, GameState) ]
allMoves' g@(GameState posMap curPlayer colorToMove won _) 
  | isJust won = []
  | isJust colorToMove = allMovesWithPiece g curPlayer $ posMap BM.!> (curPlayer, fromJust colorToMove)
  | otherwise = concatMap (allMovesWithPiece g curPlayer) $ map fst $ filter (\(_,(p,_)) -> p == curPlayer ) $ BM.toList posMap

{-# INLINE allMovesWithPiece #-}
allMovesWithPiece :: GameState -> PlayerId -> Position -> [ (Move, GameState ) ]
allMovesWithPiece gs@(GameState posMap _ _ _ _) curPlayer initPos = result
  where
    goDir d = takeWhile isFreeCell $ tail $ iterate (+d) initPos
    isFreeCell c@(x,y) = x >= 0 && x < fieldSize && 
      y >= 0 && y < fieldSize && isNothing (BM.lookup c posMap)
    dy = if curPlayer == Black then 1 else (-1)
    possibleTargets = goDir (1, dy) ++ goDir (0, dy) ++ goDir (-1, dy)
    possibleMoves = map (Move initPos) possibleTargets
    resultingStates = map (fst . acceptPlayerDecision gs . MakeMove) possibleMoves
    result = zip possibleMoves resultingStates

{-# INLINE allMoves #-}
allMoves :: GameState -> [ ((PlayerId, Color), Move) ]
allMoves g@(GameState posMap curPlayer colorToMove won _) = concatMap allMovesWithPiece' (BM.toList posMap)
  where
    allMovesWithPiece' (pos, w@(p,_)) = map (\(m,_)->(w,m)) $ allMovesWithPiece g p pos

winScore :: Int
winScore = 100000

{-# INLINE evaluateDummy #-}
evaluateDummy :: GameState -> Int
evaluateDummy (GameState posMap _ _ won turnNo) 
  | isJust won = if (fromJust won == Black) then winScore - turnNo else (-winScore + turnNo)
  | otherwise = 0


{-# INLINE evaluate #-}
evaluate :: GameState -> Int
evaluate (GameState posMap _ _ won turnNo) 
  | isJust won = if (fromJust won == Black) then winScore - turnNo else (-winScore + turnNo)
  | otherwise = sum (map score $ BM.toList posMap) - 56
  where
    score ((x,y),(p,c)) = unCoord y 

{-# INLINE evaluate' #-}
evaluate' :: GameState -> Int
evaluate' g@(GameState posMap _ _ won turnNo)
  | isJust won = if (fromJust won == Black) then winScore - turnNo else (-winScore + turnNo)
  | otherwise = sum (map pieceActivity $ BM.toList posMap)
  where
    sgn Black = id
    sgn White = negate

    pieceActivity (pos, (p,_))
      | isAttacking = sgn p 3
      | otherwise = sgn p ( 2 * max colors 1 + (unCoord $ abs $ snd pos - homeRow ))
      where 
        targetRow = if p == Black then fieldSize - 1 else 0
        homeRow = if p == Black then 0 else fieldSize - 1
        pieceMoves = allMovesWithPiece g p pos
        isBlocked = null pieceMoves
        isAttacking = any (\(Move _ (_,y),_) -> y == targetRow) pieceMoves
        colors = S.size $ S.fromList $ map (\(Move _ p2,_) -> colorMap M.! p2 ) pieceMoves

bestMove :: GameState -> (Move, Int)
bestMove g = (if' toMax maximumBy minimumBy) (comparing snd) scoredMovesSmart
  where
    toMax = isMax g
    nextPosFun = map snd . allMoves'
    possibleMoves = allMoves' g
    nodesDummy = 2500000 `div` length possibleMoves
    --scoredMovesDummy = map (over _2 (\g -> alphabeta isMax g nextPosFun evaluateDummy nodesDummy)) possibleMoves `using` parList rdeepseq
    scoresDummy = map ((\g -> alphabeta isMax g nextPosFun evaluateDummy nodesDummy) . snd) possibleMoves `using` parList rdeepseq

    nonLoosing = map snd $ filter ( if' toMax ((-winScore+100) <) (winScore-100 >) . fst ) (zip scoresDummy possibleMoves)

    nodesSmart = 100000 `div` length nonLoosing
    scoredMovesSmart =  if null nonLoosing then zip (map fst possibleMoves) scoresDummy else
      map (over _2 (\g -> alphabeta isMax g nextPosFun evaluate' nodesSmart)) nonLoosing `using` parList rdeepseq

isMax :: GameState -> Bool
isMax (GameState _ p _ _ _) = p == Black
