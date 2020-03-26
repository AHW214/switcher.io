module Main where


--------------------------------------------------------------------------------
import           Control.Monad         (filterM, unless)
import           Data.Char             (toLower)
import           System.Directory      (doesFileExist, getCurrentDirectory,
                                        listDirectory, removeFile)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure, exitSuccess)
import           System.FilePath       (FilePath, isExtensionOf)


--------------------------------------------------------------------------------
import           Option                (Options, hasIrreversible, parseOptions,
                                        tryGetExtension, tryGetUndo)
import           Switch                (generateSwitches, readSwitches,
                                        serializeSwitches, showSwitches, switch)
import           Util                  (partitionM)


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
  putStrLn ""

  unless (response `elem` confirmation) $ do
    putStrLn goodbye
    exitSuccess


--------------------------------------------------------------------------------
askForYes :: String -> IO ()
askForYes question =
  askToContinue question "Aborting..." [ "y", "yes" ]


--------------------------------------------------------------------------------
runSwitch :: Options -> FilePath -> IO ()
runSwitch opts dir = do
  progName <- getProgName

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
    putStrLn $ "Done! These are the switches I made:\n\n"
              ++ showSwitches switches

    unless (hasIrreversible opts) $ do
      mapName <- serializeSwitches switches
      putStrLn $ "Switches written to '" ++ mapName ++ "'"


--------------------------------------------------------------------------------
runUndo :: Options -> FilePath -> FilePath -> IO ()
runUndo opts switchFile dir = do
  switches <- readSwitches switchFile

  case switches of
    Nothing ->
      putStrLn "Invalid switch file"

    Just ( [], _ ) ->
      putStrLn "None of the original files are present"

    Just ( include, exclude ) -> do
      unless (null exclude) $
        putStrLn $ "Switches that cannot be performed due to missing files:\n\n"
                  ++ showSwitches exclude

      putStrLn $ "Switches that will be performed:\n\n"
                ++ showSwitches include

      askForYes "Type 'y(es)' to confirm the (un)switcheroos"

      putStrLn "Swapping..."
      switch include
      removeFile switchFile
      putStrLn $ "Done!"


--------------------------------------------------------------------------------
main :: IO ()
main =
  getArgs >>= run
  where
    run args =
      case parseOptions args of
        Right opts ->
          getCurrentDirectory
          >>= command opts
          >> exitSuccess

        Left err ->
          putStrLn err
          >> exitFailure

    command opts =
      case tryGetUndo opts of
        Just switchFile ->
          runUndo opts switchFile

        _ ->
          runSwitch opts
