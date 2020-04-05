module Main where


--------------------------------------------------------------------------------
import           Control.Monad      (unless)
import           Data.Char          (toLower)
import           Data.Functor       ((<&>))
import           System.Directory   (getCurrentDirectory, removeFile)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure, exitSuccess)


--------------------------------------------------------------------------------
import           FileSystem         (FileSystem)
import qualified FileSystem         as FS
import           Option
import           Switch             (Switches)
import qualified Switch             as SW
import           Util               (FileName, atLeast, both, whenJust)


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
numItemsInFolders :: Foldable t => FileSystem (t a) -> ( Int, Int )
numItemsInFolders =
  foldl count ( 0, 0 )
  where
    count acc@( numItems, numFolders ) items =
      if null items then
        acc
      else
        ( numItems + length items, numFolders + 1 )


--------------------------------------------------------------------------------
findFilesToSwitch :: FileSystem [ FileName ] -> Maybe (FileSystem [ FileName ])
findFilesToSwitch =
  pruneEmpty . fmap atLeastTwo
  where
    pruneEmpty =
      FS.prune (not . null)

    atLeastTwo xs =
      if atLeast 2 xs then
        xs
      else
        []


--------------------------------------------------------------------------------
runSwitch :: Options -> IO ()
runSwitch opts = do
  dir <- getCurrentDirectory

  buildOp <-
    case tryGetExtension opts of
      Just ext -> do
        putStrLn $ "Searching for files with extension '" ++ ext ++ "'..."
        return $ FS.buildWithExt ext

      _ -> do
        askForYes $ "No extension given. Might be an especially bad idea\n"
                    ++ "Type 'y(es)' if you would like to proceed:"

        putStrLn "Searching for files..."
        return FS.build

  let depth = getRecursive opts

  buildOp depth dir <&> findFilesToSwitch >>=
    \case
      Nothing ->
        putStrLn "Could not find at least two files to switch"
      Just fs -> do
        let ( numFiles, numFolders ) = numItemsInFolders fs

        putStrLn $ "Found " ++ show numFiles ++ " files in "
                  ++ show numFolders ++ " folders"
        askForYes "Type 'y(es)' to confirm the switcheroo:"

        putStrLn "Switching files..."
        switches <- mapM SW.generate fs
        FS.runWith_ SW.switch switches
        putStrLn $ "Done! These are the switches I made:\n\n"
                  ++ SW.display switches

        unless (hasIrreversible opts) $ do
          switchFile <- SW.serialize switches
          putStrLn $ "Switches written to '" ++ switchFile ++ "'"


--------------------------------------------------------------------------------
prepareUndo :: FileSystem Switches
  -> IO (Maybe (FileSystem Switches), Maybe (FileSystem Switches))
prepareUndo =
  fmap (both pruneEmpty . FS.unzip) . FS.runWith SW.sanitize
  where
    pruneEmpty =
      FS.prune (not . SW.isEmpty)


--------------------------------------------------------------------------------
runUndo :: FilePath -> IO ()
runUndo switchFile =
  SW.load switchFile >>=
    \case
      Left err ->
        putStrLn err

      Right switches ->
        prepareUndo switches >>=
          \case
            ( Nothing, _ ) ->
              putStrLn "None of the original files are present"

            ( Just include, exclude ) -> do
              whenJust exclude $ \ex ->
                putStrLn $ "Switches that cannot be performed due to missing files:\n\n"
                          ++ SW.display ex

              putStrLn $ "Switches that will be performed:\n\n"
                        ++ SW.display include

              askForYes "Type 'y(es)' to confirm the (un)switcheroos:"

              putStrLn "Switching files back..."
              FS.runWith_ SW.switch include
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
          runUndo switchFile

        _ ->
          runSwitch opts
