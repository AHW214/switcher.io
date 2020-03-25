module Main where


--------------------------------------------------------------------------------
import           Control.Monad         (filterM, foldM, unless)
import           Data.Char             (toLower)
import           System.Directory      (doesFileExist, getCurrentDirectory,
                                        listDirectory)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure, exitSuccess)
import           System.FilePath       (FilePath, isExtensionOf)


--------------------------------------------------------------------------------
import           Option                (hasIrreversible, parseOptions,
                                        tryGetExtension)
import           Switch                (generateSwitches, serializeSwitches,
                                        switch)


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
    switches <- generateSwitches files
    switch switches
    putStrLn "Done!"

    unless (hasIrreversible opts) $ do
      mapName <- serializeSwitches switches files
      putStrLn $ "Switches written to '" ++ mapName ++ "'"

  exitSuccess
  where
    askForYes question =
      askToContinue question "Aborting..." [ "y", "yes" ]
