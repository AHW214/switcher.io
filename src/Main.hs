module Main where


--------------------------------------------------------------------------------
import           Control.Monad         (filterM, foldM, forM_)
import           System.Console.GetOpt
import           System.Directory      (doesFileExist, getCurrentDirectory,
                                        listDirectory, renameFile)
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure, exitSuccess)
import           System.FilePath       (FilePath, isExtensionOf)


--------------------------------------------------------------------------------
import           Src.Random                (shuffleList)


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
partitionM :: (a -> IO Bool) -> [ a ] -> IO ( [ a ], [ a ] )
partitionM predicate =
  foldM update ( [], [] )
  where
    update ( pass, fail ) x = do
      res <- predicate x
      return $
        if res then
          ( x:pass, fail )
        else
          ( pass, x:fail )


--------------------------------------------------------------------------------
findFilesWhere :: (FilePath -> IO Bool) -> FilePath -> IO [ FilePath ]
findFilesWhere predicate dir = do
  items <- listDirectory dir
  ( files, dirs ) <- partitionM doesFileExist items

  filterM predicate files


--------------------------------------------------------------------------------
switchFileNames :: String -> String -> IO ()
switchFileNames ext dir = do
  files <- findFilesWhere (return . isExtensionOf ext) dir
  shuffled <- shuffleList files

  let pairs = zip files shuffled

  forM_ pairs
    (\( f1, f2 ) -> do
        renameFile f1 "bob"
        renameFile f2 f1
        renameFile "bob" f2
    )


--------------------------------------------------------------------------------
main :: IO ()
main = do
  parsed <- parseOptions <$> getArgs

  case parsed of
    Right opts ->
      switchFileNames "txt" =<< getCurrentDirectory

    Left err ->
      ioError $ userError err
