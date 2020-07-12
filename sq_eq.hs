{-# OPTIONS -Wall #-}

main :: IO ()
main =
  print $ solveSquare a b c
  where
    a = 1 :: Double
    b = -2
    c = 1

-- | Solve equation of kind @a x² + b x + c = 0@
solveSquare :: Double -> Double -> Double -> [Double]
solveSquare a b c
  | a == 0, b == 0 = []
  | a == 0         = [- c / b]
  | d < 0          = []
  | otherwise      = [ (- b +- sqrt d) / (2 * a)
                     | (+-) <- [(-), (+)]
                     ]
  where
    d = b * b - 4 * a * c
