Lambda Calculus
===============


Syntax
------

E ::= V           -- variable
   |  (E E)       -- application
   |  (\ V -> E)  -- abstraction

> type VarName = String

> data Expr
>   = Variable VarName
>   | Application Expr Expr
>   | Abstraction VarName Expr

< instance Show Expr


Semantics
---------

(\x -> ((pow x) 2))
\x -> ((pow x) 2)
\x -> (pow x) 2


Alpha (α)
---------

(\x -> x) z  ==  z
(\y -> y) z  ==  z

(\x -> x)  ==  (\y -> y)
(\x -> x² + 2x + 1)  ==  (\a -> a² + 2a + 1)  -- alpha-equivalence


Beta (β)
--------

(\x -> x) z  ↦  z  -- результат (редукция)

(\x -> x² + 2x + 1) 0
== 0² + 2*0 + 1
== 1

((\x -> E[x]) a)  ==  E[x := a]  -- beta-reduction
                          ^подстановка


Eta (η)
-------

f  =  \x -> x + x
g  =  \x -> x * 2

(∀ x. f x == g x)  <=>  f == g  -- eta-reduction

(\x -> x) z  ==  z  ==  (\y -> y) z
(\x -> x) z     ==      (\y -> y) z
(\x -> x)       ==      (\y -> y)  -- eta-reduction

f == g  =>  f x == g x  -- eta-expansion

x == y  =>  f x == f y

(\x -> f x) y  ==  f y  -- applied beta-reduction
 \x -> f x     ==  f    -- applied eta-reduction


Haskell Function Definition
---------------------------

f  =  \x -> x + x  -- def 1

        f x == (\x -> x + x) x == x + x
        f x == x + x

f x  =  x + x  -- def 2


discriminant a b c = b * b - 4 * a * c  -- def 3

        ((discriminant a) b) c = b * b - 4 * a * c
        (discriminant a) b = \c -> b * b - 4 * a * c
        discriminant a = \b -> \c -> b * b - 4 * a * c
        discriminant = \a -> \b -> \c -> b * b - 4 * a * c

discriminant = \a b c         -> b * b - 4 * a * c  -- def 4

f x  =  ((+) y) x
f    =  ((+) y)


discriminant a b c = b * b - 4 * a * c
discriminant a = \b c -> b * b - 4 * a * c

discriminant 0 = (\a b c -> b * b - 4 * a * c) 0
               = (\a -> \b c -> b * b - 4 * a * c) 0
               = \b c -> b * b - 4 * 0 * c
               = \b c -> b * b - 0
               = \b c -> b * b

discriminant' (a, b, c) = b * b - 4 * a * c
discriminant' = \ (a, b, c) -> b * b - 4 * a * c
discriminant' = \t -> (proj2 t) * (proj2 t) - 4 * (proj1 t) * (proj3 t)


f1 (x, y) = e[x, y]
f2  x  y  = e[x, y]  -- curried

curry f1 = f2
         = \x y -> e[x, y]
         = \x y -> f1 (x, y)

curry = \f1 x y -> f1 (x, y)  -- lambda
curry f1 x y = f1 (x, y)      -- equation
curry3 f1 x y z = f1 (x, y, z)      -- equation

uncurry f2 = f1
           = \ (x, y) -> e[x, y]
           = \ (x, y) -> f2 x y

uncurry = \f2 (x, y) -> f2 x y  -- lambda
uncurry f2 (x, y) = f2 x y      -- equation


discriminant == curry3 discriminant'
discriminant' == uncurry3 discriminant

Haskell Type
------------

f1 :: Integer -> Integer
f1 x = x + x

f2 x y = x + y
f2 x = \y -> x + y
f2 x :: Integer -> Integer
f2 :: Integer -> (Integer -> Integer)

discriminant :: Double -> (Double -> (Double -> Double))
discriminant :: Double ->  Double ->  Double -> Double


Operator Sections
-----------------

(?) :: a -> b -> c

x :: a
y :: b

x ? y :: c
(x ?) = \y -> x ? y == (?) x  -- section, сечение
(? y) = \x -> x ? y

(- y) = negate y
(x -) = (-) x

subtractThisFromX = \y -> x - y = (x -)
subtractYFromThis = \x -> x - y = subtract y

subtract y x = x - y

id = \x -> x  -- I

const = \x y -> x  -- K
