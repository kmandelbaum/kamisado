{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Kamisado 
( Color(..),Coord(..), Position, PlayerId(..), GameState(..), fieldSize, getWizzard, Move(..), isOkMove, isWon,
  GameOutput(..),PlayerDecision(..), initState, colorMap, acceptPlayerDecision, unCoord, Wizzard(..), isPlayersMove )
where

import qualified Data.Bimap as BM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Array as A
import Data.NumInstances.Tuple
import Data.Maybe

newtype Coord = Coord Int deriving (Num, Ord, Eq, Enum, Show, A.Ix, Real, Integral)

unCoord :: Coord -> Int
unCoord (Coord x) = x

type Position = (Coord, Coord)

data PlayerId = Black | White deriving (Eq, Ord, Show)

data Color = Orange | Green | Red | Magenta | Yellow | Blue | Cyan | Brown deriving (Eq, Ord, Show, Enum, Bounded)

type Wizzard = (PlayerId, Color)

type Map = M.Map
type Bimap = BM.Bimap

data GameState = GameState { _posMap :: Bimap Position Wizzard, _curPlayer :: PlayerId, _colorToMove :: Maybe Color, _wonBy :: Maybe PlayerId } deriving (Show)

data Move = Move Position Position deriving (Eq, Show)

data PlayerDecision = MakeMove Move deriving ( Eq, Show )

data GameOutput = OKMove Move | IllegalMove deriving ( Eq, Show )

isOkMove :: GameOutput -> Bool
isOkMove (OKMove _) = True
isOkMove _ = False

fieldSize :: Coord
fieldSize = 8

maxCoord :: Coord
maxCoord = fieldSize - 1

coords :: [Coord]
coords = [0..maxCoord]

nextPlayer :: PlayerId -> PlayerId
nextPlayer White = Black
nextPlayer Black = White

colorMap :: Map Position Color
colorMap = M.fromList $ zip (coords >>= zip coords . repeat) rows
  where
    f = (A.listArray range [1, 3, 5, 7, 1, 3, 5, 7] A.!)
    goNext r = A.elems $ A.array range $ zipWith ( \x i -> (i +++ f x, x) ) r coords
    rows = concatMap ( map toColor ) $ take (unCoord fieldSize) $ iterate goNext coords
    toColor = (A.listArray range [ Brown, Cyan, Blue, Yellow, Magenta, Green, Red, Orange ] A.!)
    (+++) x y = (x + y) `mod` fieldSize
    range = (0,maxCoord)

initState :: GameState
initState = GameState posMap Black Nothing Nothing
  where 
    posMap = BM.fromList $ row 0 Black ++ row (fieldSize-1) White
    row y p = map ( \x -> ( (x,y), (p, colorMap M.! (x, y)) ) ) coords

acceptPlayerDecision :: GameState -> PlayerDecision -> (GameState, GameOutput)
acceptPlayerDecision g@(GameState posMap curPlayer colorToMove won) (MakeMove m@(Move fromPos toPos))
  | isJust won = (g, IllegalMove)
  | isLegalBoard && isLegalWiz && isLegalWay = (GameState newPosMap newPlayer (Just newColor) newWon, OKMove m)
  | otherwise = (g, IllegalMove)
  where
    posDelta@(posDeltaX, posDeltaY) = toPos - fromPos
    isLegalBoard = isInside toPos && posDeltaY /= 0 && 
                   ((posDeltaX == 0) || (abs posDeltaX == abs posDeltaY)) && 
                   ((posDeltaY > 0) == (curPlayer == Black))
            
    wiz' = BM.lookup fromPos posMap
    wiz@(wizPlayer, wizColor) = fromJust wiz'
    isLegalWiz = isJust wiz' &&
                 (isNothing colorToMove || (fromJust colorToMove == wizColor)) 
                 && curPlayer == wizPlayer

    dy = signum posDeltaY
    dx = signum posDeltaX

    isLegalWay = and $ map (flip BM.notMember posMap) $ map (\i -> (fst fromPos + dx * i, snd fromPos + dy * i)) [1..abs posDeltaY]

    isInside p = isJust $ M.lookup p colorMap
    newPosMap = BM.delete fromPos $ BM.insert toPos wiz posMap
    nextWizzard = checkRepetition S.empty (nextPlayer curPlayer, colorMap M.! toPos)

    (newPlayer, newColor) = if isJust nextWizzard then fromJust nextWizzard else (nextPlayer curPlayer, fromJust colorToMove)
    
    newWon = if curPlayer == Black && snd toPos == fieldSize - 1 then Just Black
        else if curPlayer == White && snd toPos == 0 then Just White
        else if isNothing nextWizzard then Just newPlayer else Nothing

    canMove w@(p,c) = or $ map (flip BM.notMember newPosMap) $ zip posXs posYs
      where (posX,posY) = newPosMap BM.!> w
            posXs = filter ( \x -> x >=0 && x < fieldSize ) [posX, posX - 1, posX + 1]
            posYs = repeat $ if p == Black then posY + 1 else posY - 1

    checkRepetition s w@(p,c) 
      | S.member w s = Nothing
      | otherwise = if (canMove w) then Just w else
          checkRepetition ( S.insert w s ) (nextPlayer p, colorMap M.! (newPosMap BM.!> w))
  
getWizzard :: GameState -> Position -> Maybe Wizzard
getWizzard = (flip BM.lookup) . _posMap

isPlayersMove :: PlayerId -> GameState -> Bool
isPlayersMove p (GameState _ curPlayer _ won) = isNothing won && p == curPlayer

isWon :: GameState -> Bool
isWon (GameState _ p _ (Just wp)) = p /= wp
isWon _ = False
