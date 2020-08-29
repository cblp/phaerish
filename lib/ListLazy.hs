module ListLazy where

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' i p | i > 0 = p : replicate' (i - 1) p
               | otherwise = error "reoplicate count is less then 0"

zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y) : zip xs ys
zip' _ _ = []

enumerate' :: [a] -> [(Integer, a)]
enumerate' = zip' [0 ..]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

data Longest a b = These a b
  | This a
  | That b
    deriving (Show, Eq, Ord)

zipLongest :: [a] -> [b] -> [Longest a b]
zipLongest [] [] = []
zipLongest [] (y:ys) = That y : map That ys
zipLongest (x:xs) [] = This x : map This xs
zipLongest (x:xs) (y:ys) = These x y : zipLongest xs ys
