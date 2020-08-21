import           Control.Monad       (replicateM, when)
import           Data.Foldable       (for_, toList)
import           Data.Function       ((&))
import           Data.IORef          (IORef, modifyIORef, newIORef, readIORef)
import           Data.Traversable    (for)
import           Data.Vector         (Vector, thaw, (!), (//))
import qualified Data.Vector         as Vector
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MVector
import           Numeric.Natural     (Natural)
import           System.Random       (randomIO)

import FlatClusters (testFlatClusters)

main :: IO ()
main =
  do
    testMapL height width
    testMapV height width
    testMapM height width
    testFlatClusters height width
  where
    height = 8
    width  = 8

type Soil = Bool

type MapL = [[Soil]]

testMapL :: Int -> Int -> IO ()
testMapL height width = do
  m <- genMapL height width
  printMapL m
  print $ countClustersL m

genMapL :: Int -> Int -> IO MapL
genMapL height width =
  replicateM height $ replicateM width randomIO

printMapL :: MapL -> IO ()
printMapL m =
  putStr $
    unlines
      [ concat
          [ if cell then "██" else "░░"
          | cell <- row
          ]
      | row <- m
      ]

countClustersL :: MapL -> Natural
countClustersL m0 = go (0, 0) m0 where

  height = length m0

  width  = length $ head m0

  go (i, j) m =
    case next (i, j) of
      Nothing
        | m !! i !! j -> 1
        | otherwise   -> 0
      Just ij'
        | m !! i !! j ->
            1 + go ij' (removeCluster (i, j) m)
        | otherwise   -> go ij' m

  next (i, j)
    | j < width  - 1 = Just (i,     j + 1)
    | i < height - 1 = Just (i + 1, 0    )
    | otherwise      = Nothing

  removeCluster (i, j) m =
    m
    & removeCell (i, j)
    & tryRemove (i - 1, j    )
    & tryRemove (i,     j - 1)
    & tryRemove (i + 1, j    )
    & tryRemove (i + 1, j + 1)
    & tryRemove (i - 1, j + 1)
    & tryRemove (i + 1, j - 1)
    & tryRemove (i - 1, j - 1)
    & tryRemove (i,     j + 1)

  tryRemove (i, j) m
    | i >= 0, i < height, j >= 0, j < width,
      m !! i !! j =
        removeCluster (i, j) m
    | otherwise = m

  removeCell (i, j) m =
    modifyAt i (\row -> replaceAt j False row) m

  modifyAt i f xs =
    case splitAt i xs of
      (before, x:after) ->
        before ++ [f x] ++ after
      _ -> error "index is out of bounds"

  replaceAt i x xs = modifyAt i (const x) xs

type MapV = Vector (Vector Bool)

testMapV :: Int -> Int -> IO ()
testMapV height width = do
  m <- genMapV height width
  printMapV m
  print $ countClustersV m

genMapV :: Int -> Int -> IO MapV
genMapV height width = do
  asList <- genMapL height width
  pure $
    Vector.fromList
      [Vector.fromList row | row <- asList]

printMapV :: MapV -> IO ()
printMapV v =
  printMapL [toList row | row <- toList v]

countClustersV :: MapV -> Natural
countClustersV m0 = go (0, 0) m0 where

  height = length m0

  width  = length $ m0 ! 0

  go (i, j) m =
    case next (i, j) of
      Nothing
        | m ! i ! j -> 1
        | otherwise -> 0
      Just ij'
        | m ! i ! j ->
            1 + go ij' (removeCluster (i, j) m)
        | otherwise -> go ij' m

  next (i, j)
    | j < width  - 1 = Just (i,     j + 1)
    | i < height - 1 = Just (i + 1, 0    )
    | otherwise      = Nothing

  removeCluster (i, j) m =
    removeCell (i, j) m
    & tryRemove (i - 1, j    )
    & tryRemove (i,     j - 1)
    & tryRemove (i + 1, j    )
    & tryRemove (i,     j + 1)
    & tryRemove (i + 1, j + 1)
    & tryRemove (i - 1, j + 1)
    & tryRemove (i + 1, j - 1)
    & tryRemove (i - 1, j - 1)
    & tryRemove (i,     j + 1)

  tryRemove (i, j) m
    | i >= 0, i < height, j >= 0, j < width,
      m ! i ! j =
        removeCluster (i, j) m
    | otherwise = m

  removeCell (i, j) m =
    modifyAt i (\row -> replaceAt j False row) m

  replaceAt i x v = v // [(i, x)]

  modifyAt i f v = v // [(i, f (v ! i))]

type MapM = Vector (IOVector Bool)

testMapM :: Int -> Int -> IO ()
testMapM height width = do
  m <- genMapV height width
  printMapV m
  do
    mMut <- thawMap m
    c <- countClustersM mMut
    print c
  do
    mMut <- thawMap m
    c <- countClustersM mMut
    print c

thawMap :: MapV -> IO MapM
thawMap vv = for vv thaw

-- for :: [a]      -> (a -> IO b) -> IO  [b]
-- for :: Vector a -> (a -> IO b) -> IO (Vector b)

(+=) :: Num a => IORef a -> a -> IO ()
var += x = modifyIORef var (+ x)

{-
  when :: Bool -> IO () -> IO ()
  when cond action =
    | cond      = action
    | otherwise = pure ()
-}

countClustersM :: MapM -> IO Natural
countClustersM m =
  do
    c <- newIORef 0
    for_ [0 .. height - 1] $ \i -> do
      for_ [0 .. width - 1] $ \j -> do
        cell <- MVector.read (m ! i) j
        when cell $ do
          c += 1
          removeCluster (i, j)
    readIORef c
  where

    height = length m

    width  = MVector.length $ m ! 0

    removeCluster (i, j) = do
      removeCell (i, j)
      tryRemove (i - 1, j    )
      tryRemove (i,     j - 1)
      tryRemove (i + 1, j    )
      tryRemove (i,     j + 1)
      tryRemove (i + 1, j + 1)
      tryRemove (i - 1, j + 1)
      tryRemove (i + 1, j - 1)
      tryRemove (i - 1, j - 1)

    tryRemove (i, j)
      | i >= 0, i < height, j >= 0, j < width = do
          cell <- MVector.read (m ! i) j
          when cell $ do
            removeCluster (i, j)
      | otherwise = pure ()

    removeCell :: (Int, Int) -> IO ()
    removeCell (i, j) =
      MVector.write (m ! i) j False
