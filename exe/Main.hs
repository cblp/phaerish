import           System.Environment (getArgs, getProgName)
import           System.Exit        (die)

import           Sqeq               (solveSquare)

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  case args of
    [as, bs, cs] ->
      let
        a = read as
        b = read bs
        c = read cs
      in putStrLn $ "Solution: " ++ show (solveSquare a b c)
    _ -> die $ "Usage: " ++ prog ++ " NUMBER NUMBER NUMBER"
