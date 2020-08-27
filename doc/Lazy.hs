{-# OPTIONS -Wall #-}

-- data ListChar = Empty | Cons Char ListChar

data List a
  = Empty
  | Cons a (List a)

-- f :: List a -> _
-- f list = case list of
--   Empty -> ...
--   Cons head tail -> ...

-- length :: List a -> Integer
-- length list = case list of
--   Empty -> 0
--   Cons head tail -> 1 + length tail

data [] a
  = []
  | a : [a]

data Stream a
  = a :& Stream a

-- length :: [a] -> Integer
-- length []     = 0
-- length (x:xs) = 1 + length xs

-- [n ..] = enumFrom n

-- enumFrom n = n : enumFrom (n + 1)

-- ifThenElse :: Bool -> a -> a -> a
-- ifThenElse True  t _ = t
-- ifThenElse False _ e = e

-- if _ then _ else _

-- f = ................
--   where
--     g = ................
--     h = ................

-- repeat :: a -> [a]
-- -- repeat x = x : repeat x -- 1
-- repeat x = r      -- 2
--   where r = x : r

-- cycle :: [a] -> [a]
-- cycle xs = xs ++ cycle xs

-- (++) :: [a] -> [a] -> [a]
-- xs ++ ys = case xs of
--   []     -> ys
--   x : xx -> x : (xx ++ ys)

-- enumFrom 5
-- 5 : enumFrom 6
--     6 : enumFrom 7
--         7 : enumFrom 8
--             8 : enumFrom 9

-- printList :: Show a => [a] -> IO ()

-- -- equations
-- printList []     = pure () -- 1
-- printList (x:xs) = do      -- 2
--   print x
--   printList xs

-- -- equation + case
-- printList xs = case xs of
--   []     -> pure ()
--   (x:xs) -> do
--     print x
--     printList xs

-- -- map :: (a -> b) -> [a] -> [b]
-- map _ []     = []
-- map f (x:xs) = f x : map f xs

-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ []                 = []
-- filter p (x:xs) | p x       = x : filter p xs
--                 | otherwise =     filter p xs


-- printList xs = for_ xs print

-- (*>) :: f a -> f b -> f b

-- for_ :: Applicative f => [a] -> (a -> f b) -> f ()
-- for_ []     action = pure ()
-- for_ (x:xs) action = (action x :: f b) *> (for_ xs action :: f ()) :: f ()

for :: Applicative f => [a] -> (a -> f b) -> f [b]
for []     _      = pure []
for (x:xs) action =
--   (liftA2 (:)) (action x) (for xs action)
--    liftA2 (:)  (action x) (for xs action)
--    liftA2 (:)  (action x) $ for xs action
--   pure (:) <*> action x <*> for xs action
  (:) <$> action x <*> for xs action

-- for (x:xs) action = (g <*> action x) <*> (for xs action :: f [b])
--                           :: f [b]
--   where
--     -- <*> :: f (c -> d) -> f c -> f d
--     action x :: f b
--     (g <*> action x) :: f ([b] -> [b])
--     g :: f (b -> [b] -> [b])
--     g = pure (:)

-- for (x:xs) action = pure (:) <*> action x <*> for xs action

-- (:)        ::   b ->   [b] ->   [b]
-- liftA2 (:) :: f b -> f [b] -> f [b]

liftA2 ::
  Applicative f =>
  (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = pure f <*> x <*> y

(<$>) :: Applicative f => (a -> b) -> f a -> f b
f <$> x = pure f <*> x
