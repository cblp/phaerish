module FlatClusters (testFlatClusters) where

import Control.Monad (replicateM, when)
import Data.Foldable (for_, toList)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Vector (Vector, freeze, thaw)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVector
import Numeric.Natural (Natural)
import System.Random (randomIO)
import Prelude hiding (read)

testFlatClusters :: Int -> Int ->  IO ()
testFlatClusters height width = do
  m <- genMap width height
  v <- newIOVector2 width m
  printMap m width
  count <- countClusters v
  print count

data IOVector2 a = IOVector2
  { width :: Int,
    cells :: IOVector a
  }

read :: IOVector2 a -> (Int, Int) -> IO a
read IOVector2 {width = width, cells = cells} (i, j) =
  MVector.read cells (i * width + j)

genMap :: Int -> Int -> IO [Bool]
genMap width height = replicateM (width * height) randomIO

newIOVector2 :: Int -> [Bool] -> IO (IOVector2 Bool)
newIOVector2 width m = do
  cells <- thaw $ Vector.fromList m
  return $ IOVector2 {width = width, cells = cells}

printMap :: [Bool] -> Int -> IO ()
printMap m width = do
  putStr $
    unlines
      [ concat
          [if cell then "██" else "░░" | cell <- row]
        | row <- cells
      ]
  where
    cells = splitEvery width m

countClusters :: IOVector2 Bool -> IO Natural
countClusters v@IOVector2 {width = width, cells = cells} =
  do
    c <- newIORef 0
    for_ [0 .. height - 1] $ \i -> do
      for_ [0 .. width - 1] $ \j -> do
        cell <- read v (i, j)
        when cell $ do
          modifyIORef c (+ 1)
          removeCluster (i, j)
    readIORef c
  where
    height = MVector.length cells `quot` width

    removeCluster (i, j) = do
      removeCell (i, j)
      tryRemove (i - 1, j)
      tryRemove (i, j - 1)
      tryRemove (i + 1, j)
      tryRemove (i, j + 1)

    tryRemove (i, j)
      | i >= 0,
        i < height,
        j >= 0,
        j < width = do
        cell <- read v (i, j)
        when cell $ do
          removeCluster (i, j)
      | otherwise = pure ()

    removeCell :: (Int, Int) -> IO ()
    removeCell (i, j) =
      MVector.write cells (i * width + j) False

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list