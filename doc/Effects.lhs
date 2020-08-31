> {-# LANGUAGE InstanceSigs #-}

-- $> :set -XNoStarIsType

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

% > instance Functor [] where
% >   fmap :: (a -> b) -> [a] -> [b]
% >   fmap = map

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

% > instance Functor (Either c) where
% >   fmap :: (a -> b) -> Either c a -> Either c b
% >   fmap f e = case e of
% >     Left c  -> Left c
% >     Right a -> Right (f a)

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

< type Reader a b = a -> b

% > data Reader a b = Reader (a -> b)

% > runReader :: Reader a b -> a -> b
% > runReader (Reader f) = f

% {- $>
%   isTemperatureGood =
%     Reader $ \t -> t > 0 && t < 30
% <$ -}

% -- $> runReader isTemperatureGood 10

% -- $> runReader isTemperatureGood 100

% > instance Functor (Reader c) where
% >   fmap :: (a -> b) -> Reader c a -> Reader c b
% >   fmap f (Reader g) = Reader $ f . g

% {- $>
%   isTemperatureBad = not <$> isTemperatureGood
% <$ -}

% -- $> runReader isTemperatureBad 10

% -- $> runReader isTemperatureBad 100

Недетерминированность — вариативность
-------------------------------------

< data [] a = [] | a : [a]

% -- $> d6 = [1..6]

% -- $> (2 *) <$> d6

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

% > instance Applicative (Either c) where
% >
% >   pure :: a -> Either c a
% >   pure = Right
% >
% >   (<*>)
% >     :: Either c (a -> b)
% >     -> Either c a
% >     -> Either c b
% >   Right f <*> Right x = Right $ f x
% >   Left  c <*> _       = Left c
% >   _       <*> Left c  = Left c

% -- $> liftA2 (+) (Left "Nope") (Right 212)

% -- $> liftA2 (+) (Right 213) (Right 214)
