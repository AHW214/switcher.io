module Main where


--------------------------------------------------------------------------------
import           Control.Monad         (unless)
import           Data.Char             (toLower)
import           System.Directory      (doesDirectoryExist, getCurrentDirectory,
                                        removeFile)
import           System.Environment    (getArgs, getExecutablePath)
import           System.Exit           (exitFailure, exitSuccess)
import           System.FilePath       (FilePath)


--------------------------------------------------------------------------------
import qualified FileSystem            as FS
import           Option
import           Switch
import           Util                  (partitionM)


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
  exePath <- getExecutablePath

  buildOp <-
    case tryGetExtension opts of
      Just ext -> do
        putStrLn $ "Searching for files with extension '" ++ ext ++ "'..."
        return (FS.buildWithExt ext)

      _ -> do
        putStrLn "No extension given. Might be an especially bad idea"
        askForYes "Type 'y(es)' if you would like to proceed:"

        return FS.build

  let depth = getRecursive opts
  maybeFs <- sanitize exePath <$> buildOp depth dir

  case maybeFs of
    Nothing ->
      putStrLn "Could not find at least two files to switch"
    Just fs -> do
      let ( numFiles, numFolders ) = numItems fs

      putStrLn $ "Found " ++ show numFiles ++ " files in "
                ++ show numFolders ++ " folders"
      askForYes "Type 'y(es)' to confirm the switcheroo:"

      putStrLn "Swapping..."
      switches <- generateSwitches fs
      switch switches
      putStrLn $ "Done! These are the switches I made:\n\n"
                ++ showSwitches dir switches

      unless (hasIrreversible opts) $ do
        switchFile <- serializeSwitches switches dir
        putStrLn $ "Switches written to '" ++ switchFile ++ "'"
  where
    numItems =
      foldl (\num@( numFiles, numFolders ) fs ->
        case fs of
          [] ->
            num

          _ ->
            ( numFiles + length fs, numFolders + 1 )
      ) ( 0, 0 )

    sanitize exePath =
      FS.filter (not . null)
      . fmap atLeastTwo
      . FS.mapFilter (exePath /=)

    atLeastTwo xs =
      if length xs < 2 then
        []
      else
        xs


--------------------------------------------------------------------------------
runUndo :: Options -> FilePath -> FilePath -> IO ()
runUndo opts switchFile _ = do
  maybeSwitches <- readSwitches switchFile

  case maybeSwitches of
    Nothing ->
      putStrLn "Invalid switch file"

    Just switches -> do
      let dir = FS.getRootLabel switches
      dirExists <- doesDirectoryExist dir

      unless dirExists $ do
        putStrLn $ "Switches made in directory '" ++ dir -- make prettier
                  ++ "'\nwhich no longer exists"
        exitSuccess

      ( include, exclude ) <- prepareUndo switches

      if FS.isEmpty include then
        putStrLn "None of the original files are present"
      else do
        unless (FS.isEmpty exclude) $
          putStrLn $ "Switches that cannot be performed due to missing files:\n\n"
                    ++ showSwitches dir exclude

        putStrLn $ "Switches that will be performed:\n\n"
                  ++ showSwitches dir include

        askForYes "Type 'y(es)' to confirm the (un)switcheroos:"

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
