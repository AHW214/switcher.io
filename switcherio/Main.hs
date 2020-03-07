module Main where


--------------------------------------------------------------------------------
import           Control.Monad         (filterM, foldM, forM_)
import           Data.Maybe            (listToMaybe, mapMaybe)
import           System.Console.GetOpt
import           System.Directory      (doesFileExist, getCurrentDirectory,
                                        listDirectory, renameFile)
import           System.Environment    (getArgs)
import           System.Exit           (exitFailure, exitSuccess)
import           System.FilePath       (FilePath, isExtensionOf)


--------------------------------------------------------------------------------
import           SwitcherIO.Random                (randomRSequence, shuffleList)


--------------------------------------------------------------------------------
data Flag
  = Recursive
  | Extension String
  deriving (Eq, Show)


--------------------------------------------------------------------------------
hasRecursive :: [ Flag ] -> Bool
hasRecursive = any (== Recursive)


--------------------------------------------------------------------------------
fromExtension :: Flag -> Maybe String
fromExtension flag =
  case flag of
    Extension ext ->
      Just ext

    _ ->
      Nothing

getExtension :: [ Flag ] -> Maybe String
getExtension = listToMaybe . mapMaybe fromExtension


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
findFilesWithExt :: String -> FilePath -> IO [ FilePath ]
findFilesWithExt ext =
  findFilesWhere (return . isExtensionOf ext)


--------------------------------------------------------------------------------
switchFileNames :: [ FilePath ] -> IO ()
switchFileNames files =
   zip files
   <$> shuffleList files
   >>= mapM_ switcheroo
    where
      switcheroo ( file1, file2 ) = do
        temp <- makeTempName 10

        renameFile file1 temp
        renameFile file2 file1
        renameFile temp file2

      makeTempName =
        randomRSequence ( 'A', 'z' )


--------------------------------------------------------------------------------
main :: IO ()
main = do
  parsed <- parseOptions <$> getArgs

  case parsed of
    Right opts ->
      case getExtension opts of
        Just ext -> do
          dir <- getCurrentDirectory
          files <- findFilesWithExt ext dir

          let len = length files

          if len <= 0 then do
            putStrLn $ "no files with extension '" ++ ext ++ "' found"
            exitSuccess
          else do
            putStrLn $ "found " ++ show len ++ " files\n"
                      ++ "are you sure you want to continue (y/n)"

            response <- getLine

            if response == "yes" || response == "y" then do
              putStrLn "swapping..."
              switchFileNames files
              putStrLn "done!"
            else do
              putStrLn "aborting..."
              exitSuccess

        _ ->
          fail "no extension provided"

    Left err ->
      fail err
