module Grid (
  Grid2D, GridLocation2D,
  generateGrid, fromLists, getRow, gridSize, get2D, update2D, bulkUpdate2D,
  GridWalker(..),
  walkRows, walkColumns, walkColumnsRev,
  mapGrid, mapGridM, mapGridM_, imapGrid, imapGridM, imapGridM_, zipGrid, unzipGrid,
  GridUpdater(..), updateColumns, updateColumnsRev,
  eitherGrid
) where

--------------------------------------------------------------------------------
import Control.Applicative

import Data.Either
import Data.Maybe (fromJust)
import Data.Monoid

import qualified Data.Vector as V
--------------------------------------------------------------------------------

newtype Grid2D a = Grid2D { getGrid :: V.Vector (V.Vector a) }

instance Functor Grid2D where
  f `fmap` (Grid2D g) = Grid2D $ V.map (V.map f) g

type GridLocation2D = (Int, Int)

gridSize :: Grid2D a -> (Int, Int)
gridSize (Grid2D grid)
  | glength == 0 = (0, 0)
  | otherwise = (glength, V.length $ V.head grid)
  where
    glength = V.length grid

generateGrid :: Int -> Int -> (Int -> Int -> a) -> Grid2D a
generateGrid cols rows f = Grid2D $ V.generate cols (\col -> V.generate rows (\row -> f col row))

toLists :: Grid2D a -> [[a]]
toLists = V.toList . (V.map V.toList) . getGrid

fromLists :: [[a]] -> Grid2D a
fromLists = Grid2D . V.fromList . (map V.fromList)

getRow :: Int -> Grid2D a -> [a]
getRow row = V.toList . V.map (V.! (row - 1)) . getGrid

mapGrid :: (a -> b) -> Grid2D a -> Grid2D b
mapGrid = fmap

mapGridM :: (Functor m, Monad m) => (a -> m b) -> Grid2D a -> m (Grid2D b)
mapGridM f g = Grid2D <$> (V.mapM (V.mapM f) $ getGrid g)

mapGridM_ :: (Functor m, Monad m) => (a -> m b) -> Grid2D a -> m ()
mapGridM_ f g = mapGridM f g >> return ()

imapGrid :: (Int -> Int -> a -> b) -> Grid2D a -> Grid2D b
imapGrid f = Grid2D . V.imap (\col -> V.imap (\row -> f col row)) . getGrid

imapGridM :: (Functor m, Monad m) => ((Int, Int) -> a -> m b) -> Grid2D a -> m (Grid2D b)
imapGridM f grid = mapGridM (uncurry f) (zipGrid locGrid grid)
  where
    locGrid = uncurry generateGrid (gridSize grid) (,)

imapGridM_ :: (Functor m, Monad m) => ((Int, Int) -> a -> m b) -> Grid2D a -> m ()
imapGridM_ f g = imapGridM f g >> return ()

zipGrid :: Grid2D a -> Grid2D b -> Grid2D (a, b)
zipGrid (Grid2D f) (Grid2D g) = Grid2D $ V.zipWith V.zip f g

unzipGrid :: Grid2D (b, c) -> (Grid2D b, Grid2D c)
unzipGrid = (\(x, y) -> (Grid2D x, Grid2D y)) . V.unzip . (V.map V.unzip) . getGrid

get2D :: GridLocation2D -> Grid2D a -> a
get2D (x, y) (Grid2D b) = (b V.! (x - 1)) V.! (y - 1)

update2D :: a -> GridLocation2D -> Grid2D a -> Grid2D a
update2D val (x, y) (Grid2D g) = let
  col = g V.! (x - 1)
  newcol = col V.// [((y - 1), val)]
  in
   Grid2D $ g V.// [((x - 1), newcol)]

bulkUpdate2D :: a -> [GridLocation2D] -> Grid2D a -> Grid2D a
bulkUpdate2D val = flip $ foldr (update2D val)

data GridWalker a b = Result b
                    | Walker (Maybe a -> GridWalker a b)

stepWalker :: GridWalker a b -> a -> GridWalker a b
stepWalker (Result b) _ = Result b
stepWalker (Walker f) x = f (Just x)

finishWalker :: GridWalker a b -> b
finishWalker (Result b) = b
finishWalker (Walker f) = finishWalker $ f Nothing

walkRows :: Grid2D a -> GridWalker a b -> [b]
walkRows (Grid2D grid) walker
  | V.length grid == 0 = []
  | otherwise = let
    step :: V.Vector (GridWalker a b) -> V.Vector a -> V.Vector (GridWalker a b)
    step = V.zipWith stepWalker
    in
     V.toList $ V.map finishWalker $ V.foldl' step (V.map (\_ -> walker) (V.head grid)) grid

walkColumns :: Grid2D a -> GridWalker a b -> [b]
walkColumns (Grid2D grid) walker = V.toList $ V.map (finishWalker . (V.foldl' stepWalker walker)) grid

walkColumnsRev :: Grid2D a -> GridWalker a b -> [b]
walkColumnsRev (Grid2D grid) walker = V.toList $ V.map (finishWalker . (V.foldr' (flip stepWalker) walker)) grid

newtype GridUpdater a b = GridUpdater { updateGridValue :: a -> (b, GridUpdater a b) }

updateScanFn :: a -> (Maybe b, GridUpdater a b) -> (Maybe b, GridUpdater a b)
updateScanFn x (_, GridUpdater fn) = let (y, next) = fn x in (Just y, next)

updateColumns :: GridUpdater a b -> Grid2D a -> Grid2D b
updateColumns updater = Grid2D . V.map updateColumn . getGrid
  where updateColumn = V.map (fromJust . fst) . V.postscanl' (flip updateScanFn) (Nothing, updater)

updateColumnsRev :: GridUpdater a b -> Grid2D a -> Grid2D b
updateColumnsRev updater = Grid2D . V.map updateColumn . getGrid
  where updateColumn = V.map (fromJust . fst) . V.postscanr' updateScanFn (Nothing, updater)

eitherGrid :: Monoid e => Grid2D (Either e a) -> Either e (Grid2D a)
eitherGrid grid = let
  collectEithers :: Monoid e => Either e a -> Maybe (Either e a) ->
                    GridWalker (Either e a) (Either e a)
  collectEithers x Nothing = Result x
  collectEithers (Right _) (Just (Left y)) = Walker $ collectEithers (Left y)
  collectEithers (Left x) (Just (Left y)) = Walker $ collectEithers (Left $ x `mappend` y)
  collectEithers (Right x) (Just (Right _)) = Walker $ collectEithers (Right x)
  collectEithers (Left x) (Just (Right _)) = Walker $ collectEithers (Left x)

  mapEither :: Monoid e => Either e a -> e
  mapEither (Left x) = x
  mapEither _ = mempty

  result = walkRows grid $ Walker $ collectEithers (Right undefined)
  in
   if (any isLeft result)
   then Left . mconcat $ map mapEither result
   else Right $ mapGrid (either undefined id) grid
