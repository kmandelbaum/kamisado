module AI where

import Utility
import qualified Debug.Trace as DT

alphabeta :: ( Ord score, Bounded score, Show position ) => Bool -> position -> ( position -> [ position ] ) -> ( position -> score ) -> Int -> score
alphabeta isMaximize pos moves evaluate nodes = dynamic
  where
    dynamic = fst $ optimize isMaximize pos minBound maxBound nodes
    optimize isMax pos' alpha beta maxNodes 
      | maxNodes <= 1 || null allMoves = (evaluate pos', 0)
      | otherwise = go ( if isMax then alpha else beta, maxNodes) allMoves
      where
        allMoves = moves pos'
        go x [] = x
        go (s, n) (p:ps) | (if' isMax (s >= beta) (s <= alpha) ) = (s, n)
                         | otherwise = let (s', n') = optimize (not isMax) p (if' isMax s alpha) (if' isMax beta s) (n `div` (length ps + 1)) 
                                           partn = n `div` (length ps + 1)
                                        in go (if' isMax max min s s', n' + n - partn) ps
