import           Control.Applicative ((<**>))
import           Options.Applicative (Parser, ParserInfo, argument, auto,
                                      execParser, fullDesc, header, help,
                                      helper, info, long, metavar, option,
                                      short, value)

import           Sqeq                (solveSquare)

data Options = Options
  { a, b, c  :: Double
  , language :: Language
  }

data Language = En | Ru
  deriving (Read, Show)

main :: IO ()
main = do
  Options{a = a, b = b, c = c, language = language} <- execParser parserInfo
  let solution =
        case language of
          En -> "Solution"
          Ru -> "Ответ"
  putStrLn $ solution ++ ": " ++ show (solveSquare a b c)

parserInfo :: ParserInfo Options
parserInfo =
  info
    (parser <**> helper)
    (fullDesc <> header "Solve square equation")

parser :: Parser Options
parser =
  Options
    <$> argument auto (metavar "A" <> help "x^2 coefficient")
    <*> argument auto (metavar "B" <> help "x coefficient")
    <*> argument auto (metavar "C" <> help "free term")
    <*> option   auto (   metavar "LANGUAGE"
                      <>  help "Language: En (default), Ru"
                      <>  value En
                      <>  long "language"
                      <>  short 'l'
                      )
