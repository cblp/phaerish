main :: IO ()
main = do
  putStrLn "Hello there"

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' i p = p : replicate' (i - 1) p

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip xs ys

enumerate' :: [a] -> [(Integer, a)]
enumerate' = zip' [0 ..]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

data Longest a b = These a b
  | This a
  | That b
    deriving (Show)

zipLongest :: [a] -> [b] -> [Longest a b]
zipLongest [] [] = []
zipLongest [] (y:ys) = That y : zipLongest [] ys
zipLongest (x:xs) [] = This x : zipLongest xs []
zipLongest (x:xs) (y:ys) = These x y : zipLongest xs ys
