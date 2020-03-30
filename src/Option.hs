module Option
  ( Flag
  , Options
  , getRecursive
  , hasIrreversible
  , parseOptions
  , tryGetExtension
  , tryGetUndo
  ) where


--------------------------------------------------------------------------------
import           Data.Maybe             (fromMaybe, listToMaybe, mapMaybe)
import           System.Console.GetOpt
import           Text.Read              (readMaybe)


--------------------------------------------------------------------------------
data Flag
  = Irreversible
  | Extension String
  | Undo String
  | Recursive (Maybe String)
  deriving (Eq)


--------------------------------------------------------------------------------
type Options = [ Flag ]


--------------------------------------------------------------------------------
tryGetOption :: (Flag -> Maybe a) -> Options -> Maybe a
tryGetOption fromFlag =
  listToMaybe . mapMaybe fromFlag


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
getRecursive :: Options -> Int
getRecursive =
  fromMaybe 0 . tryGetOption fromRecursive
  where
    fromRecursive flag =
      case flag of
        Recursive level ->
          fromLevel level
        _ ->
          Nothing

    fromLevel level =
      case level >>= readMaybe of
        Nothing ->
          Just (-1)

        lvl ->
          lvl


--------------------------------------------------------------------------------
options :: [ OptDescr Flag ]
options =
  [ Option ['r'] ["recursive"]    (OptArg Recursive "NUMBER")    "recurse into subdirectories"
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
