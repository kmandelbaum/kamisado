module AI where

import Utility

{-# INLINE alphabeta #-}
alphabeta :: ( Ord score, Show position ) => (position -> Bool) -> position -> ( position -> [ position ] ) -> ( position -> score ) -> score -> score -> Int -> score
alphabeta isMaxFun pos moves evaluate minScore maxScore nodes = dynamic
  where
    dynamic = fst $ optimize pos minScore maxScore nodes
    optimize pos' alpha beta maxNodes 
      | maxNodes <= 1 || null allMoves = (evaluate pos', 0)
      | otherwise = go ( if isMax then alpha else beta, maxNodes) allMoves (length allMoves)
      where
        isMax = isMaxFun pos'
        allMoves = moves pos'
        go x [] _ = x
        go (s, n) (p:ps) l | (if' isMax (s >= beta) (s <= alpha) ) = (s, n)
                           | otherwise = let (s', n') = optimize p (if' isMax s alpha) (if' isMax beta s) partn
                                             partn = n `div` l
                                         in go (if' isMax max min s s', n' + n - partn) ps (l-1)
