module Option
  ( Flag
  , Options
  , hasIrreversible
  , hasRecursive
  , parseOptions
  , tryGetExtension
  , tryGetUndo
  ) where


--------------------------------------------------------------------------------
import           Data.Maybe             (listToMaybe, mapMaybe)
import           System.Console.GetOpt


--------------------------------------------------------------------------------
data Flag
  = Recursive
  | Irreversible
  | Extension String
  | Undo String
  deriving (Eq)


--------------------------------------------------------------------------------
type Options = [ Flag ]


--------------------------------------------------------------------------------
tryGetOption :: (Flag -> Maybe a) -> Options -> Maybe a
tryGetOption fromFlag =
  listToMaybe . mapMaybe fromFlag


--------------------------------------------------------------------------------
hasRecursive :: Options -> Bool
hasRecursive = elem Recursive


--------------------------------------------------------------------------------
hasIrreversible :: Options -> Bool
hasIrreversible = elem Irreversible


--------------------------------------------------------------------------------
tryGetUndo :: Options -> Maybe String
tryGetUndo = tryGetOption fromUndo
  where
    fromUndo flag =
      case flag of
        Undo switchFile ->
          Just switchFile

        _ ->
          Nothing


--------------------------------------------------------------------------------
tryGetExtension :: Options -> Maybe String
tryGetExtension = tryGetOption fromExtension
  where
    fromExtension flag =
      case flag of
        Extension ext ->
          Just ext

        _ ->
          Nothing


--------------------------------------------------------------------------------
options :: [ OptDescr Flag ]
options =
  [ Option ['r'] ["recursive"]    (NoArg Recursive)              "recurse into subdirectories"
  , Option ['i'] ["irreversible"] (NoArg Irreversible)           "don't serialize the switch"
  , Option ['e'] ["extension"]    (ReqArg Extension "EXTENSION") "specify type of files to switch"
  , Option ['u'] ["undo"]         (ReqArg Undo "FILE")           "undo a particular set of switches"
  ]


--------------------------------------------------------------------------------
parseOptions :: [ String ] -> Either String Options
parseOptions args =
  case getOpt Permute options args of
    ( opts, _, [] ) ->
      Right opts

    ( _, _, errs ) ->
      Left $ concat errs ++ usageInfo header options
  where
    header =
      "Usage: switch [OPTION...]"
