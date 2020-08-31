> {-# LANGUAGE InstanceSigs #-}

-- $> :set -XFlexibleContexts -XNoStarIsType

> import           Prelude hiding (
>                    Applicative(..),
>                    Either(..),
>                    Functor(..),
>                    Maybe(..),
>                    (<$>),
>                  )

> import           Data.Vector (Vector)
> import qualified Data.Vector as Vector

--------------------------------------------------

% -- $> map (+ 1) [3, 15, 9, 20]

% {- $>
%   Vector.map (+ 1) $
%     Vector.fromList [3, 15, 9, 20]
% <$ -}

% -- $> :t map

% -- $> :t map (+ 1)

--------------------------------------------------

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

> instance Functor [] where
>   fmap :: (a -> b) -> [a] -> [b]
>   fmap = map

% > instance Functor Vector where
% >   fmap :: (a -> b) -> Vector a -> Vector b
% >   fmap = Vector.map

% -- $> :t fmap (+ 1)

> (<$>) :: Functor f => (a -> b) -> f a -> f b
> (<$>) = fmap

% -- $> (+ 1) <$> [3, 15, 9, 20]

% -- $> (+ 1) <$> Vector.fromList [3, 15, 9, 20]

--------------------------------------------------

Чистота
=======

-   Определённость/вычислимость/завершимость

    ∃ y = f x

    counterexample:

        2/0

--------------------------------------------------

-   Детерминированность

    f x = f x

    counterexample:

        > random()
        < 3

        > random()
        < 15

        > random()
        < 9

        > random()
        < 20

    example:

        -- pseudorandom
        > g <- newStdGen

        > g
        < 2147483398

        > random @Char g
        < ('ы', 2147442707)

        > random @Char g
        < ('ы', 2147442707)

--------------------------------------------------

-   Отсутствие побочных эффектов

    counterexample:

        appendFile
          "/var/log/myapp.log"
          "INFO: something happened"

--------------------------------------------------

Нельзя, но очень хочется
========================

Неопределённость/Невычислимость/Незавершимость
----------------------------------------------

∃ y = f x

> data Maybe a = Nothing | Just a
>   deriving (Show)

> instance Functor Maybe where
>   fmap :: (a -> b) -> Maybe a -> Maybe b
>   fmap f m = case m of
>     Nothing -> Nothing
>     Just a  -> Just (f a)

% -- $> (+ 1) <$> Nothing

% -- $> (+ 1) <$> Just 100

--------------------------------------------------

> data Either a b = Left a | Right b
>   deriving (Show)

% -- $> :kind Either

% -- $> :kind Either Bool Char

% -- $> :kind Either Bool

> instance Functor (Either c) where
>   fmap :: (a -> b) -> Either c a -> Either c b
>   fmap f e = case e of
>     Left c  -> Left c
>     Right a -> Right (f a)

% {- $>
%   (+ 1) <$>
%   Left
%     "These aren't the droids you're looking for"
% <$ -}

% -- $> (+ 1) <$> Right 114

--------------------------------------------------

Детерминированность
-------------------

    f x = f x

Недетерминированность — неявная зависимость
-------------------------------------------

< type Reader r a = r -> a

> data Reader r a = Reader (r -> a)

> runReader :: Reader r a -> r -> a
> runReader (Reader f) = f

{- $>
  isTemperatureGood =
    Reader $ \t -> t > 0 && t < 30
<$ -}

% -- $> runReader isTemperatureGood 10

% -- $> runReader isTemperatureGood 100

> instance Functor (Reader r) where
>   fmap :: (a -> b) -> Reader r a -> Reader r b
>   fmap f (Reader g) = Reader $ f . g

{- $>
  isTemperatureBad = not <$> isTemperatureGood
<$ -}

% -- $> runReader isTemperatureBad 10

% -- $> runReader isTemperatureBad 100

Недетерминированность — вариативность
-------------------------------------

< data [] a = [] | a : [a]

-- $> d6 = [1..6]

-- $> roll2d6 = (2 *) <$> d6

-- $> roll2d6

--------------------------------------------------

Чистота             │ Эффект        │ Тип     │ F
━━━━━━━━━━━━━━━━━━━━┿━━━━━━━━━━━━━━━┿━━━━━━━━━┿━━
Завершимость        │ Остановка     │ Maybe   │ +
                    │               │ Either  │ +
────────────────────┼───────────────┼─────────┼──
Детерминированность │ Зависимость   │ Reader  │ +
                    ├───────────────┼─────────┼──
                    │ Вариативность │ []      │ +

--------------------------------------------------

> class Applicative f where
>
>   pure :: a -> f a
>
>   (<*>) :: f (a -> b) -> f a -> f b
>   (<*>) = liftA2 id
>
>   liftA2 :: (a -> b -> c) -> f a -> f b -> f c
>   liftA2 f x y = pure f <*> x <*> y

Неопределённость/Невычислимость/Незавершимость
----------------------------------------------

> instance Applicative Maybe where
>
>   pure :: a -> Maybe a
>   pure = Just
>
>   (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
>   Just f <*> Just x = Just $ f x
>   _      <*> _      = Nothing

% -- $> liftA2 (+) Nothing (Just 212)

% -- $> liftA2 (+) (Just 213) (Just 214)

<       (<$>)   :: (a -> b) -> f a -> f b
<      f <$> ax :: f b

< pure          :: a -> f a
< pure          :: (a -> b) -> f (a -> b)
<       (<*>)   :: f (a -> b) -> f a -> f b
< pure f <*> ax :: f b

< f <$> ax == pure f <*> ax

-- $> (+) <$> Nothing <*> Just 212

-- $> (+) <$> Just 213 <*> Just 214

--------------------------------------------------

> instance Applicative (Either c) where
>
>   pure :: a -> Either c a
>   pure = Right
>
>   (<*>)
>     :: Either c (a -> b)
>     -> Either c a
>     -> Either c b
>   Right f <*> Right x = Right $ f x
>   Left  c <*> _       = Left c
>   _       <*> Left c  = Left c

-- $> (+) <$> Left "Nope" <*> Right 212

-- $> (+) <$> Right 213 <*> Right 214

--------------------------------------------------

Чистота           │ Эффект        │ Тип     │ F A
━━━━━━━━━━━━━━━━━━┿━━━━━━━━━━━━━━━┿━━━━━━━━━┿━━━━━
Завершимость      │ Остановка     │ Maybe   │ + ➕
                  │               │ Either  │ + ➕
──────────────────┼───────────────┼─────────┼─────
Детерминированн.  │ Зависимость   │ Reader  │ +
                  ├───────────────┼─────────┼─────
                  │ Вариативность │ []      │ +

--------------------------------------------------

> instance Applicative (Reader r) where
>
>   pure :: a -> Reader r a
>   pure a = Reader $ \r -> a
>
>   (<*>)
>     :: Reader r (a -> b)
>     -> Reader r a
>     -> Reader r b
>   Reader f <*> Reader x =
>     Reader $ \r -> (f r) (x r)

-- $> :t (,)

-- $> (,) "foo" "bar"

{- $>
  isTemperatureGoodOrBad =
    (||)
    <$> isTemperatureGood
    <*> isTemperatureBad
<$ -}

-- $> runReader isTemperatureGoodOrBad 10

-- $> runReader isTemperatureGoodOrBad 100

--------------------------------------------------

Чистота           │ Эффект        │ Тип     │ F A
━━━━━━━━━━━━━━━━━━┿━━━━━━━━━━━━━━━┿━━━━━━━━━┿━━━━━
Завершимость      │ Остановка     │ Maybe   │ + +
                  │               │ Either  │ + +
──────────────────┼───────────────┼─────────┼─────
Детерминированн.  │ Зависимость   │ Reader  │ + ➕
                  ├───────────────┼─────────┼─────
                  │ Вариативность │ []      │ +

--------------------------------------------------

> instance Applicative [] where
>
>   pure :: a -> [a]
>   pure x = [x]
>
>   (<*>) :: [a -> b] -> [a] -> [b]
>   fs <*> xs = [f x | f <- fs, x <- xs]

-- $> roll2d6 = (+) <$> d6 <*> d6

-- $> roll2d6

-- $> Data.List.nub roll2d6

--------------------------------------------------

Чистота           │ Эффект        │ Тип     │ F A
━━━━━━━━━━━━━━━━━━┿━━━━━━━━━━━━━━━┿━━━━━━━━━┿━━━━━
Завершимость      │ Остановка     │ Maybe   │ + +
                  │               │ Either  │ + +
──────────────────┼───────────────┼─────────┼─────
Детерминированн.  │ Зависимость   │ Reader  │ + +
                  ├───────────────┼─────────┼─────
                  │ Вариативность │ []      │ + ➕

--------------------------------------------------

Побочные эффекты
================

Неявный результат

> data Writer w a = Writer (a, w)
>   deriving (Show)

{- $>
  addWithLog x y =
    Writer (x + y, ["x = " <> show x, "y = " <> show y])
<$ -}

-- $> addWithLog 3 15

--------------------------------------------------

> instance Functor (Writer w) where
>   fmap :: (a -> b) -> Writer w a -> Writer w b
>   fmap f (Writer (a, w)) = Writer (f a, w)

-- $> show <$> addWithLog 3 15

-- $> show <$> ((+ 1) <$> addWithLog 3 15)

--------------------------------------------------

< instance Applicative (Writer w) where
<
<   pure :: a -> Writer w a
<   pure a = Writer (a, _)

--------------------------------------------------

< class Monoid m where
<   (<>) :: m -> m -> m
<   mempty :: m

-- $> mempty :: String

-- $> "hell" <> "o"

--------------------------------------------------

> instance Monoid w => Applicative (Writer w)
>   where
>
>   pure :: a -> Writer w a
>   pure a = Writer (a, mempty)
>
>   (<*>)
>     :: Writer w (a -> b)
>     -> Writer w a
>     -> Writer w b
>   Writer (f, w1) <*> Writer (x, w2) =
>     Writer (f x, w1 <> w2)

> makeWithLog name value =
>   Writer (value, [name <> " = " <> show value])

-- $> x = makeWithLog "x" 415

-- $> y = makeWithLog "y" 419

-- $> (+) <$> x <*> y

--------------------------------------------------

Чистота           │ Эффект        │ Тип     │ F A
━━━━━━━━━━━━━━━━━━┿━━━━━━━━━━━━━━━┿━━━━━━━━━┿━━━━━
Завершимость      │ Остановка     │ Maybe   │ + +
                  │               │ Either  │ + +
──────────────────┼───────────────┼─────────┼─────
Детерминированн.  │ Зависимость   │ Reader  │ + +
                  ├───────────────┼─────────┼─────
                  │ Вариативность │ []      │ + +
──────────────────┼───────────────┼─────────┼─────
Нет побочки       │ Лишний выход  │ Writer  │ ➕➕

--------------------------------------------------
