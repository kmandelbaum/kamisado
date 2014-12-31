{-# LANGUAGE TypeFamilies,DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts, RecursiveDo, BangPatterns #-}
import Graphics.QML
import Data.Text (Text)
import FRP.Sodium
import qualified Data.Text as T
import Data.Proxy
import Data.Typeable
import System.IO
import System.Random
import Control.Monad
import Data.Functor
import Kamisado
import qualified Data.Map as M
import qualified Data.Bimap as BM
import Variadic
import Data.Bits
import Control.Applicative
import Data.Maybe
import KamisadoAI
import Utility
import Control.Concurrent

aiPlayerID :: PlayerId
aiPlayerID = White

(<@>) :: Behavior (a -> b) -> Event a -> Event b
(<@>) = flip $ snapshot (flip ($)) 
infixl 4 <@>

(<@) :: Behavior b -> Event a -> Event b
(<@) = flip $ snapshot (flip const)
infixl 4 <@

(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<.>) = liftA2 (.)
infixl 4 <.>

data FieldCell = FieldCell { _cellX::Int, _cellY::Int, _cellCol::Color } deriving Typeable
data Piece = Piece { _pieceX::Int, _pieceY::Int, _pieceColor::Color, _piecePlayer::PlayerId } deriving (Typeable,Show)

instance Marshal Coord where
  type MarshalMode Coord c d = ModeTo c
  marshaller = toMarshaller (\(Coord x) -> x)

instance Marshal Color where
  type MarshalMode Color c d = ModeTo c
  marshaller = toMarshaller colorMarshallerTo
    where colorMarshallerTo = T.toLower . T.pack . show 

instance Marshal FieldCell where
  type MarshalMode FieldCell c d = ModeObjFrom FieldCell c
  marshaller = fromMarshaller fromObjRef

instance DefaultClass FieldCell where
  classMembers = [ defPropertyConst "x" ( return . _cellX),
                   defPropertyConst "y" ( return . _cellY),
                   defPropertyConst "color" ( return . _cellCol) ]

instance Marshal Piece where
  type MarshalMode Piece c d = ModeObjFrom Piece c
  marshaller = fromMarshaller fromObjRef

instance DefaultClass Piece where
  classMembers = [ defPropertyConst "x" ( return . _pieceX ),
                   defPropertyConst "y" ( return . _pieceY ),
                   defPropertyConst "fillColor" ( return . _pieceColor ),
                   defPropertyConst "lineColor" ( return . T.toLower . T.pack . show . _piecePlayer ) ]

fieldModel :: IO [ObjRef FieldCell]
fieldModel = do
  mapM (\((Coord x,Coord y), z) -> newObjectDC $ FieldCell x y z ) $ M.assocs colorMap

marshalList :: (Show a, DefaultClass a) => [a] -> IO [ObjRef a]
marshalList  = mapM newObjectDC

data ButtonsMask = ButtonsMask { isLeftPressed :: Bool, isRightPresses ::  Bool, isMiddlePressed :: Bool } deriving Show

data Mouse = Mouse { mouseX :: Int, mouseY :: Int, mousePressed :: ButtonsMask } deriving (Show)

mkMouseEvent :: Int -> Int -> Int -> Return Mouse
mkMouseEvent x y buttonsInt = Return $ Mouse x y $ ButtonsMask (bit 0) (bit 1) (bit 2)
  where bit = testBit buttonsInt

mkCellSizeChangedEvent :: Int -> Return Int
mkCellSizeChangedEvent = Return

defReactiveMethod' :: ( Marshal tt, Typeable tt, FunSuffix a es, MethodSuffix(ChangeRtn es (IO ())))
                        => String -> es -> Reactive (Member tt, Event a)
defReactiveMethod' name e = do
  (ev, putVal) <- newEvent
  return (defMethod' name (\_->fsmap (sync . putVal) e), ev)

type MouseEvents = (Event Mouse, Event Mouse, Event Mouse)
data Drag = Drag { _dragGamePos::Position, _dragScreenPos:: (Int, Int) } deriving Show

changeGameState :: GameState -> Move -> Maybe GameState
changeGameState g m = ifF (isOkMove . snd) (Just . fst) (const Nothing) $
  acceptPlayerDecision g (MakeMove m)

playGame :: MouseEvents -> Behavior Int -> Event () -> GameState -> Reactive (Behavior GameState, Behavior [Piece])
playGame (eMouseDown, eMouseUp, eMouseMove) bCellSize eNewGame i = do 
  let
    toCell s (x,y) = (Coord $ x `div` s, Coord $ y `div` s)
    toDrag g s m@(Mouse x y _) = let p = toCell s (x,y) in Drag p (x,y) <$ getWizzard g p
    toXY (Mouse x y _) = (x,y)
    toPieces g drag m cellSize = map piece $ BM.toList (_posMap g)
      where 
        piece (p@(Coord x, Coord y), (player, color)) = Piece (x * cellSize + dx) (y * cellSize + dy) color player
          where
            (dx,dy) = if Just p == (_dragGamePos <$> drag) then (m - (_dragScreenPos $ fromJust drag)) else (0,0)
        
    eMouse = eMouseMove `merge` eMouseDown `merge` eMouseUp
  rec
    bGameState <- hold i $ eGameState `merge` (const i <$> eNewGame)
    bDrag <- hold Nothing $ (Just <$> eDrag) `merge` (const Nothing <$> eDrop)
    (eAIMove, makeAIMove) <- newEvent
    let
      eDrag = filterJust $ toDrag <$> bGameState <*> bCellSize <@> eMouseDown
      eDrop = toCell <$> bCellSize <@> (toXY <$> eMouseUp)
      ePlayerMove = filterJust $ ( \drag drop -> (flip ( Move . _dragGamePos ) drop) <$> drag) <$> bDrag <@> eDrop
      eGameState = filterJust $ changeGameState <$> bGameState <@> ( ePlayerMove `merge` eAIMove )
      eAIStartThinking = filterJust $ ( ifF ((||False).isPlayersMove aiPlayerID) Just (const Nothing) ) <$> eGameState
      aiThread g = do
        let !(m,n) = bestMove g
        print (m,n)
        sync $ makeAIMove m

  bMouseXY <- hold (0,0) $ (\(Mouse x y _) -> (x,y)) <$> eMouse
  listen eAIStartThinking (\g -> forkIO (aiThread g) >> return ())
  listen ePlayerMove print
  let bPieces = toPieces <$> bGameState <*> bDrag <*> bMouseXY <*> bCellSize

  return (bGameState, bPieces) 

main :: IO ()
main = do
    (mouseDownMethod, eMouseDown :: Event Mouse) <- sync $ defReactiveMethod' "mouseDown" mkMouseEvent
    (mouseUpMethod, eMouseUp :: Event Mouse) <- sync $ defReactiveMethod' "mouseUp" mkMouseEvent
    (mouseMoveMethod, eMouseMove :: Event Mouse) <- sync $ defReactiveMethod' "mouseMove" mkMouseEvent
    (newGameMethod, eNewGame :: Event () ) <- sync $ defReactiveMethod' "newGame" ( Return () )

    (cellSizeChangedMethod, eCellSizeChanged :: Event Int) <- sync $ defReactiveMethod' "cellSizeChanged" mkCellSizeChangedEvent 

    bCellSize <- sync $ hold 1 (filterE (/=0) eCellSizeChanged )

    (bGameState, bPieces) <- sync $ playGame (eMouseDown, eMouseUp, eMouseMove) bCellSize eNewGame initState

    let eventMethods = [mouseDownMethod, mouseUpMethod, mouseMoveMethod, cellSizeChangedMethod, newGameMethod]

    ( skvFieldModel :: SignalKey (IO()) ) <- newSignalKey
    ( skvPiecesModel :: SignalKey (IO()) ) <- newSignalKey

    clazz <- newClass $ [defPropertySigRO' "fieldModel" skvFieldModel (\_ -> fieldModel),
                         defPropertySigRO' "piecesModel" skvPiecesModel (\_ -> (sync $ sample bPieces) >>= marshalList ),
                         defPropertyConst' "fieldSizeX" (\_ -> return fieldSize),
                         defPropertyConst' "fieldSizeY" (\_ -> return fieldSize) ] ++ eventMethods
    ctx <- newObject clazz ()

    sync $ listen (updates bPieces) (const $ fireSignal skvPiecesModel ctx)

    runEngineLoop defaultEngineConfig {
            initialDocument = fileDocument "qml/main.qml",
            contextObject = Just $ anyObjRef ctx}
