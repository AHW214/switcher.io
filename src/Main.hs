module Main where


--------------------------------------------------------------------------------
import           System.Console.GetOpt
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)


--------------------------------------------------------------------------------
data Flag
  = Recursive
  | Extension String
  deriving (Show)


--------------------------------------------------------------------------------
options :: [ OptDescr Flag ]
options =
  [ Option ['r'] ["recursive"] (NoArg Recursive)              "recurse into subdirectories"
  , Option ['e'] ["extension"] (ReqArg Extension "EXTENSION") "type of files to swap"
  ]


--------------------------------------------------------------------------------
parseOptions :: [ String ] -> Either String [ Flag ]
parseOptions args =
  case getOpt Permute options args of
    ( opts, _, [] ) ->
      Right opts

    ( _, _, errs ) ->
      Left $ concat errs ++ usageInfo header options
    where
      header = "Usage: switch -e <EXTENSION> [OPTION...]"


--------------------------------------------------------------------------------
main :: IO ()
main = do
  parsed <- parseOptions <$> getArgs

  case parsed of
    Right opts ->
      putStrLn $ show opts

    Left err ->
      ioError $ userError err
