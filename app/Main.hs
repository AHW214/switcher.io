module Main where


--------------------------------------------------------------------------------
import           Control.Monad         (unless)
import           Data.Char             (toLower)
import           System.Directory      (getCurrentDirectory, removeFile)
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

      putStrLn $ FS.drawMany fs

      putStrLn $ "Found " ++ show numFiles ++ " files in "
                ++ show numFolders ++ " folders"
      askForYes "Type 'y(es)' to confirm the switcheroo:"

      putStrLn "Swapping..."
      switches <- generateSwitches fs
      switch switches
      putStrLn $ "Done! These are the switches I made:\n\n"
                ++ showSwitches switches

      {-
      unless (hasIrreversible opts) $ do
        mapName <- serializeSwitches switches dir
        putStrLn $ "Switches written to '" ++ mapName ++ "'"
      -}

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

{-
runUndo :: Options -> FilePath -> FilePath -> IO ()
runUndo opts switchFile dir = do
  switches <- readSwitches switchFile

  case switches of
    Nothing ->
      putStrLn "Invalid switch file"

    Just fs -> do
      unless (prevDir == dir) $
        askForYes $ "The current directory is '" ++ dir ++ "'\n"
                    ++ "These switches were made in the directory '" ++ prevDir ++ "'\n"
                    ++ "Type 'y(es)' to proceed anyway:"

      ( include, exclude ) <- prepareUndo switches

      if null include then
        putStrLn "None of the original files are present"
      else do
        unless (null exclude) $
          putStrLn $ "Switches that cannot be performed due to missing files:\n\n"
                    ++ showSwitches exclude

        putStrLn $ "Switches that will be performed:\n\n"
                  ++ showSwitches include

        askForYes "Type 'y(es)' to confirm the (un)switcheroos:"

        putStrLn "Swapping..."
        switch include
        removeFile switchFile
        putStrLn $ "Done!"
-}

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
      runSwitch opts
      {-
      case tryGetUndo opts of
        Just switchFile ->
          runUndo opts switchFile

        _ ->
          runSwitch opts
      -}
