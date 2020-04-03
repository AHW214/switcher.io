module Switch
  ( Switch
  , generate
  , sanitize
  , load
  , serialize
  , display
  , switch
  , switchPairwise
  ) where


--------------------------------------------------------------------------------
import           Control.Exception (tryJust)
import           Control.Monad     (guard, (<=<))
import           Data.List         (sortBy)
import           Data.Ord          (comparing)
import           Data.Tuple        (swap)
import           System.Directory  (doesFileExist)
import           System.FilePath   (takeFileName)
import           System.IO.Error   (isDoesNotExistError)
import           Text.Read         (readMaybe)


--------------------------------------------------------------------------------
import           FileSystem       (FileSystem)
import qualified FileSystem       as FS
import           Random           (randomRSequence, shuffleList)
import           Util             (FileName, createUnique, directoryHasFile,
                                   partitionM, renameInDirectory)


--------------------------------------------------------------------------------
type Switch =
  ( FileName, FileName )


--------------------------------------------------------------------------------
makeTempName :: FilePath -> Int -> IO FileName
makeTempName dir len =
  createUnique (directoryHasFile dir) (const $ randomRSequence ( 'A', 'Z' ) len)


--------------------------------------------------------------------------------
generate :: [ FileName ] -> IO [ Switch ]
generate files =
  zip files <$> shuffleList files


--------------------------------------------------------------------------------
serialize :: FileSystem [ Switch ] -> IO FilePath
serialize switches = do
  fileName <- makeFileName
  writeFile fileName $ show switches
  return fileName
  where
    makeFileName =
      createUnique doesFileExist $ return . ("switch" ++) . show


--------------------------------------------------------------------------------
load :: FilePath -> IO (Either String (FileSystem [ Switch ]))
load path =
  validate <$> tryJust (guard . isDoesNotExistError) (readFile path)
  where
    validate =
      \case
        Left _ ->
          Left $ badFile ++ " does not exist"

        Right file ->
          case readMaybe file of
            Just switchFile ->
              Right switchFile

            _ ->
              Left $ badFile ++ " is formatted incorrectly"

    badFile =
      "Switch file '" ++ path ++ "'"


--------------------------------------------------------------------------------
sanitize :: FilePath -> [ Switch ] -> IO ( [ Switch ], [ Switch ] )
sanitize dir =
  partitionM canSwitch . invertSwitches
  where
    invertSwitches =
      map swap

    canSwitch =
      directoryHasFile dir . fst


--------------------------------------------------------------------------------
displayEach :: [ Switch ] -> [ String ]
displayEach switches =
  map showSwitch sorted
  where
    sorted =
      sortBy (comparing fst) switches

    padLen =
      maximum $ map (length . fst) switches

    padding file =
      replicate (padLen - length file) ' '

    showSwitch ( file1, file2 ) =
      file1 ++ padding file1 ++ " -> " ++ file2


--------------------------------------------------------------------------------
display :: FileSystem [ Switch ] -> String
display =
  draw . showFolders
  where
    showFolders =
      FS.mapBoth takeFileName displayEach

    draw =
      FS.drawManyWith id


--------------------------------------------------------------------------------
switch :: FilePath -> [ Switch ] -> IO ()
switch dir =
  mapM_ fromTemp <=< mapM toTemp
  where
    toTemp ( file1, file2 ) = do
      temp <- makeTempName dir 10
      rename file1 temp
      return ( temp, file2 )

    fromTemp ( temp, file2 ) =
      rename temp file2

    rename =
      renameInDirectory dir


--------------------------------------------------------------------------------
switchPairwise :: FilePath -> [ Switch ] -> IO ()
switchPairwise dir =
  mapM_ switcheroo
  where
    switcheroo ( file1, file2 ) = do
      temp <- makeTempName dir 10
      rename file1 temp
      rename file2 file1
      rename temp file2

    rename =
      renameInDirectory dir
