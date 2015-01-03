module Main where

--------------------------------------------------------------------------------
import Control.Wire hiding ((.))

import Data.Maybe (catMaybes)

import FRP.Netwire.Input

import qualified Graphics.UI.GLFW as GLFW
import qualified Lambency as L

import Linear hiding (trace)

import Grid

import Debug.Trace
--------------------------------------------------------------------------------

--------------------------------------------------
-- Constants

screenWidth :: Int
screenWidth = 450

screenHeight :: Int
screenHeight = 450

gridSz :: Int
gridSz = 15

cellSizeX :: Int
cellSizeX = screenWidth `div` gridSz

cellSizeY :: Int
cellSizeY = screenHeight `div` gridSz

delayTime :: Float
delayTime = 0.5

--------------------------------------------------
-- Types

data CellState = On | Off
               deriving (Show, Eq, Ord, Enum, Bounded)
data CellColor = Red | Green
               deriving (Show, Eq, Ord, Enum, Bounded)

type Board = Grid2D (CellState, CellColor)
type BoardFn =
  Maybe CellState -> Maybe CellState -> Maybe CellState -> Maybe CellState ->
  (CellState, CellColor) -> (CellState, CellColor)

type BoardSprites = CellState -> CellColor -> L.Sprite

--------------------------------------------------
-- Rendering

renderQuad :: L.Sprite -> V2 Float -> Float -> L.GameMonad ()
renderQuad s (V2 x y) sz =
  let hsz = sz * 0.5
      pos = V2 (x - hsz) (y - hsz)
      sc = round <$> V2 sz sz
  in L.renderSprite s sc (-1) pos

renderBoard :: BoardSprites -> Board -> L.GameMonad ()
renderBoard fn = imapGridM_ renderCell
  where
    renderCell :: (Int, Int) -> (CellState, CellColor) -> L.GameMonad ()
    renderCell (x, y) (st, c) =
      let pos = fromIntegral <$> (V2 (x * cellSizeX) (screenHeight - ((y + 1) * cellSizeY)))
      in L.renderSprite (fn st c) (V2 cellSizeX cellSizeY) (-1) pos

--------------------------------------------------
-- Logic

chooseState :: CellColor -> [CellState] -> (CellState, CellColor)
chooseState c sts
  | numOn == 2 || numOn == 3 = (On, c)
  | otherwise = (Off, c)
  where
    numOn = length $ filter (== On) sts

gameOfLife :: BoardFn
gameOfLife Nothing Nothing Nothing Nothing x = x
gameOfLife u d l r (st, Red) = chooseState Red $ catMaybes [u, d, l, r]
gameOfLife u d l r (st, Green) = chooseState Green $ catMaybes [u, d, l, r]

updateBoard :: BoardFn -> CellColor -> Board -> Board
updateBoard fn c b = imapGrid upd b
  where
    getState :: Int -> Int -> CellState
    getState x y = fst $ get2D (x + 1, y + 1) b

    upd x y pair
      | c /= (snd pair) = pair
      | otherwise = fn up down left right pair
      where
        up | y == 0 = Nothing
           | otherwise = Just $ getState x (y - 1)

        down | y == (gridSz - 1) = Nothing
             | otherwise = Just $ getState x (y + 1)

        left | x == 0 = Nothing
             | otherwise = Just $ getState (x - 1) y

        right | x == (gridSz - 1) = Nothing
              | otherwise = Just $ getState (x + 1) y

switchColor :: Board -> L.GameMonad Board
switchColor b = do
  mpos <- (\(a, b) -> V2 a b) <$> cursor
  click <- mbIsPressed GLFW.MouseButton'1
  case click of
    False -> return b
    True -> do
      releaseButton GLFW.MouseButton'1
      let V2 x' y' = floor <$> ((fromIntegral gridSz) *^ (0.5 *^ (mpos ^+^ (V2 1 1))))
          (st, c) = get2D (x' + 1, y' + 1) b
      return $ update2D ((if st == On then Off else On), c) (x' + 1, y' + 1) b

goForever :: BoardFn -> BoardSprites -> CellColor -> Board -> L.GameWire () ()
goForever fn sprites c b =
  let smallDelay = (mkGen_ $ \_ -> do
        renderBoard sprites b
        return (Right ())) >>> for delayTime
   in ((L.quitWire GLFW.Key'R >>> smallDelay) -->) $ mkGenN $ \_ -> do
    renderBoard sprites b
    stop <- keyIsPressed GLFW.Key'R
    case stop of
      True -> do
        releaseKey GLFW.Key'R
        return (Right (), gameWire fn sprites c $ updateBoard fn c b)
      False ->
        case c of
          Red -> return (Right (), goForever fn sprites Green $ updateBoard fn c b)
          Green -> return (Right (), goForever fn sprites Red $ updateBoard fn c b)      

gameWire :: BoardFn -> BoardSprites -> CellColor -> Board -> L.GameWire () ()
gameWire fn sprites c b = mkGenN $ \_ -> do
  renderBoard sprites b
  nb <- switchColor b

  keepGoing <- keyIsPressed GLFW.Key'R
  case keepGoing of
    True -> do
      releaseKey GLFW.Key'R
      return (Right (), goForever fn sprites c nb)
    False -> do
      advance <- keyIsPressed GLFW.Key'Space
      case advance of
        False -> return (Right (), gameWire fn sprites c nb)
        True -> do
          releaseKey GLFW.Key'Space
          case c of
            Red -> return (Right (), gameWire fn sprites Green $ updateBoard fn c nb)
            Green -> return (Right (), gameWire fn sprites Red $ updateBoard fn c nb)

--------------------------------------------------
-- Init

shooterCam :: L.GameWire () L.Camera
shooterCam = pure zero >>> (L.mk2DCam screenWidth screenHeight)

mkCellSprites :: IO BoardSprites
mkCellSprites = do
  red <- L.createSolidTexture (255, 0, 0, 255) >>= L.loadStaticSpriteWithTexture
  darkRed <- L.createSolidTexture (128, 0, 0, 255) >>= L.loadStaticSpriteWithTexture
  green <- L.createSolidTexture (0, 255, 0, 255) >>= L.loadStaticSpriteWithTexture
  darkGreen <- L.createSolidTexture (0, 128, 0, 255) >>= L.loadStaticSpriteWithTexture
  return $ chooseSprite red darkRed green darkGreen
  where
    chooseSprite x _ _ _ On Red = x
    chooseSprite _ x _ _ Off Red = x
    chooseSprite _ _ x _ On Green = x
    chooseSprite _ _ _ x Off Green = x

initialBoard :: Board
initialBoard =
  zipGrid (generateGrid gridSz gridSz $ \x y -> Off) $
  generateGrid gridSz gridSz $ \x y -> if even (x + y) then Red else Green

startWire :: IO (L.GameWire () ())
startWire = do
  sprites <- mkCellSprites
  return $ gameWire gameOfLife sprites Green initialBoard

loadGame :: IO (L.Game ())
loadGame = do
  w <- startWire
  return $ L.Game { L.staticLights = [],
                    L.staticGeometry = [],
                    L.mainCamera = shooterCam,
                    L.dynamicLights = [],
                    L.gameLogic = w >>> L.quitWire GLFW.Key'Q}

main :: IO ()
main = L.withWindow screenWidth screenHeight "Two-Phase Cellular Automata 15" $
       L.loadAndRun () loadGame
