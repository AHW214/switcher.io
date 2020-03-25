module Option
  ( Flag
  , Options
  , getExtension
  , hasIrreversible
  , hasRecursive
  , parseOptions
  , tryGetExtension
  ) where


--------------------------------------------------------------------------------
import           Control.Exception.Safe (Exception, MonadThrow, SomeException,
                                         throwM)
import           Data.Maybe             (listToMaybe, mapMaybe)
import           System.Console.GetOpt


--------------------------------------------------------------------------------
data Flag
  = Recursive
  | Irreversible
  | Extension String
  deriving (Eq)


--------------------------------------------------------------------------------
type Options = [ Flag ]


--------------------------------------------------------------------------------
data OptionException
  = CannotParse [ String ]
  | MissingExtension


--------------------------------------------------------------------------------
instance Show OptionException where
  show ex =
    "OptionException: " ++ message
    where
      message =
        case ex of
          CannotParse errs ->
            concat errs ++ usageInfo header options

          MissingExtension ->
            "No extension provided"

      header =
        "Usage: switch -e <EXTENSION> [OPTION...]"



instance Exception OptionException

--------------------------------------------------------------------------------
hasRecursive :: Options -> Bool
hasRecursive = elem Recursive


--------------------------------------------------------------------------------
hasIrreversible :: Options -> Bool
hasIrreversible = elem Irreversible


--------------------------------------------------------------------------------
fromExtension :: Flag -> Maybe String
fromExtension flag =
  case flag of
    Extension ext ->
      Just ext

    _ ->
      Nothing


--------------------------------------------------------------------------------
tryGetExtension :: Options -> Maybe String
tryGetExtension = listToMaybe . mapMaybe fromExtension


--------------------------------------------------------------------------------
getExtension :: MonadThrow m => Options -> m String
getExtension opts =
  case tryGetExtension opts of
    Just ext ->
      return ext

    _ ->
      throwM MissingExtension


--------------------------------------------------------------------------------
options :: [ OptDescr Flag ]
options =
  [ Option ['r'] ["recursive"]    (NoArg Recursive)              "recurse into subdirectories"
  , Option ['i'] ["irreversible"] (NoArg Irreversible)           "don't serialize the switch"
  , Option ['e'] ["extension"]    (ReqArg Extension "EXTENSION") "specify type of files to switch"
  ]


--------------------------------------------------------------------------------
parseOptions :: MonadThrow m => [ String ] -> m Options
parseOptions args =
  case getOpt Permute options args of
    ( opts, _, [] ) ->
      return opts

    ( _, _, errs ) ->
      throwM $ CannotParse errs
