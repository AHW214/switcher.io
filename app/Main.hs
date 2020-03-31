module Main where


--------------------------------------------------------------------------------
import           Control.Monad      (join, unless)
import           Data.Bifunctor     (bimap)
import           Data.Char          (toLower)
import           System.Directory   (doesDirectoryExist, getCurrentDirectory,
                                     removeFile)
import           System.Environment (getArgs, getExecutablePath)
import           System.Exit        (exitFailure, exitSuccess)
import           System.FilePath    (FilePath)


--------------------------------------------------------------------------------
import           FileSystem         (FileSystem)
import qualified FileSystem         as FS
import           Option
import           Switch             as SW
import           Util               (partitionM, whenJust)


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
pruneEmpty :: Traversable t => FileSystem (t a) -> Maybe (FileSystem (t a))
pruneEmpty =
  FS.filter (not . null)


--------------------------------------------------------------------------------
numItemsInFolders :: Traversable t => FileSystem (t a) -> ( Int, Int )
numItemsInFolders =
  foldl count ( 0, 0 )
  where
    count acc@( numItems, numFolders ) items =
      if null items then
        acc
      else
        ( numItems + length items, numFolders + 1 )


--------------------------------------------------------------------------------
findFilesToSwitch :: FileSystem [ FilePath ]
  -> IO (Maybe (FileSystem [ FilePath ]))
findFilesToSwitch fs =
  pruneEmpty . fmap atLeastTwo . flip notMe fs <$> getExecutablePath
  where
    notMe path =
      FS.mapFilter (path /=)

    atLeastTwo xs =
      case xs of { _:_:_ -> xs; _ -> [] }


--------------------------------------------------------------------------------
runSwitch :: Options -> IO ()
runSwitch opts = do
  dir <- getCurrentDirectory
  exePath <- getExecutablePath

  buildOp <-
    case tryGetExtension opts of
      Just ext -> do
        putStrLn $ "Searching for files with extension '" ++ ext ++ "'..."
        return $ FS.buildWithExt ext

      _ -> do
        askForYes $ "No extension given. Might be an especially bad idea\n"
                    ++ "Type 'y(es)' if you would like to proceed:"
        return FS.build

  let depth = getRecursive opts
  maybeFs <- findFilesToSwitch =<< buildOp depth dir

  case maybeFs of
    Nothing ->
      putStrLn "Could not find at least two files to switch"
    Just fs -> do
      let ( numFiles, numFolders ) = numItemsInFolders fs

      putStrLn $ "Found " ++ show numFiles ++ " files in "
                ++ show numFolders ++ " folders"
      askForYes "Type 'y(es)' to confirm the switcheroo:"

      putStrLn "Swapping..."
      switches <- SW.generate fs
      SW.run switches
      putStrLn $ "Done! These are the switches I made:\n\n"
                ++ SW.display dir switches

      unless (hasIrreversible opts) $ do
        switchFile <- SW.serialize switches dir
        putStrLn $ "Switches written to '" ++ switchFile ++ "'"


--------------------------------------------------------------------------------
prepareUndo :: FileSystem [ Switch ]
 -> IO (Maybe (FileSystem [ Switch ]), Maybe (FileSystem [ Switch ]))
prepareUndo =
  fmap (join bimap pruneEmpty) . SW.sanitize


--------------------------------------------------------------------------------
runUndo :: FilePath -> Options -> IO ()
runUndo switchFile opts = do
  maybeSwitches <- SW.load switchFile

  case maybeSwitches of
    Nothing ->
      putStrLn "Invalid switch file"

    Just switches -> do
      let dir = FS.getRootLabel switches
      dirExists <- doesDirectoryExist dir

      unless dirExists $ do
        putStrLn $ "Switches made in directory which no longer exists:\n'" ++ dir ++ "'"
        exitSuccess

      undo <- prepareUndo switches

      case undo of
        ( Nothing, _ ) ->
          putStrLn "None of the original files are present"

        ( Just include, exclude ) -> do
          whenJust exclude $ \ex ->
            putStrLn $ "Switches that cannot be performed due to missing files:\n\n"
                      ++ SW.display dir ex

          putStrLn $ "Switches that will be performed:\n\n"
                    ++ SW.display dir include

          askForYes "Type 'y(es)' to confirm the (un)switcheroos:"

          putStrLn "Swapping..."
          SW.run include
          removeFile switchFile
          putStrLn "Done!"


--------------------------------------------------------------------------------
main :: IO ()
main =
  getArgs >>= run
  where
    run args =
      case parseOptions args of
        Right opts ->
          command opts
          >> exitSuccess

        Left err ->
          putStrLn err
          >> exitFailure

    command opts =
      case tryGetUndo opts of
        Just switchFile ->
          runUndo switchFile opts

        _ ->
          runSwitch opts
