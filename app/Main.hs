module Main where


--------------------------------------------------------------------------------
import           Control.Monad         (filterM, foldM, forM_, unless, when)
import           Data.Char             (toLower)
import           System.Directory      (doesFileExist, getCurrentDirectory,
                                        listDirectory, renameFile)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure, exitSuccess)
import           System.FilePath       (FilePath, isExtensionOf)


--------------------------------------------------------------------------------
import           Option                (parseOptions, tryGetExtension)
import           Random                (randomRSequence, shuffleList)


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
listFilesAndDirs :: FilePath -> IO ( [ FilePath ], [ FilePath ] )
listFilesAndDirs dir =
  listDirectory dir >>= partitionM doesFileExist


--------------------------------------------------------------------------------
findFilesWhere :: (FilePath -> IO Bool) -> FilePath -> IO [ FilePath ]
findFilesWhere predicate dir =
  listFilesAndDirs dir >>= filterM predicate . fst


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
askToContinue :: String -> String -> [ String ] -> IO ()
askToContinue question goodbye confirmation = do
  putStrLn question
  response <- map toLower <$> getLine
  unless (response `elem` confirmation) $ do
    putStrLn goodbye
    exitSuccess


--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args

  dir <- getCurrentDirectory
  progName <- getProgName

  let maybeExt = tryGetExtension opts

  files <- filter (progName /=) <$>
    case tryGetExtension opts of
      Just ext -> do
        putStrLn $ "Searching for files with extension '" ++ ext ++ "'..."
        findFilesWithExt ext dir

      _ -> do
        putStrLn "No extension given. Might be an especially bad idea"
        askForYes "Type 'y(es)' if you would like to proceed..."

        fst <$> listFilesAndDirs dir

  let len = length files

  if len <= 0 then
    putStrLn "No files found"
  else if len == 1 then
    putStrLn "Only one file found, and it takes two to do a switcheroo"
  else do
    putStrLn $ "Found " ++ show len ++ " files"
    askForYes "Type 'y(es)' to confirm the switcheroo"

    putStrLn "Swapping..."
    switchFileNames files
    putStrLn "Done!"

  exitSuccess
  where
    askForYes question =
      askToContinue question "Aborting..." [ "y", "yes" ]
