module Switch
  ( Switch
  , generate
  , sanitize
  , load
  , serialize
  , display
  , run
  , runPairwise
  ) where


--------------------------------------------------------------------------------
import           Control.Monad    (join, (<=<))
import           Data.Bifunctor   (bimap)
import           Data.List        (sortBy)
import           Data.Maybe       (fromMaybe)
import           Data.Tuple       (swap)
import           System.Directory (doesFileExist, renameFile)
import           System.FilePath  (FilePath, makeRelative, takeFileName)
import           Text.Read        (readMaybe)


--------------------------------------------------------------------------------
import           FileSystem       (FileSystem)
import qualified FileSystem       as FS
import           Random           (randomRSequence, shuffleList)
import           Util             (createUnique, partitionM)


--------------------------------------------------------------------------------
type Switch =
  ( FilePath, FilePath )


--------------------------------------------------------------------------------
makeTempName :: Int -> IO FilePath
makeTempName len =
  createUnique doesFileExist (const $ randomRSequence ( 'A', 'z' ) len)


--------------------------------------------------------------------------------
generate :: FileSystem [ FilePath ] -> IO (FileSystem [ Switch ])
generate =
  mapM switchFolder
  where
    switchFolder files =
      zip files <$> shuffleList files


--------------------------------------------------------------------------------
serialize :: FileSystem [ Switch ] -> FilePath -> IO FilePath
serialize switches dir = do
  fileName <- makeFileName
  writeFile fileName $ show switches
  return fileName
  where
    makeFileName =
      createUnique doesFileExist (return . ("switch" ++) . show)


--------------------------------------------------------------------------------
load :: FilePath -> IO (Maybe (FileSystem [ Switch ]))
load =
  fmap readMaybe . readFile


--------------------------------------------------------------------------------
sanitizeFolder :: [ Switch ] -> IO ( [ Switch ], [ Switch ] )
sanitizeFolder =
  partitionM canSwitch . invertSwitches
  where
    invertSwitches =
      map swap

    canSwitch =
      doesFileExist . fst


--------------------------------------------------------------------------------
sanitize :: FileSystem [ Switch ]
  -> IO ( FileSystem [ Switch ], FileSystem [ Switch ] )
sanitize =
  fmap FS.unzip . mapM sanitizeFolder


--------------------------------------------------------------------------------
displayEach :: [ Switch ] -> [ String ]
displayEach switches =
  map showSwitch sorted
  where
    sorted =
      sortBy (\( f1, _ ) ( f2, _ ) -> f1 `compare` f2) formatted

    formatted =
      map (join bimap takeFileName) switches

    padLen =
      maximum $ map (length . fst) formatted

    padding file =
      replicate (padLen - length file) ' '

    showSwitch ( file1, file2 ) =
      file1 ++ padding file1 ++ " -> " ++ file2


--------------------------------------------------------------------------------
display :: FilePath -> FileSystem [ Switch ] -> String
display dir =
  draw . rootLabel . relativeLabels . showFolders
  where
    showFolders =
      fmap displayEach

    relativeLabels =
      FS.mapLabels (makeRelative dir)

    rootLabel =
      FS.setRootLabel dir

    draw =
      FS.drawManyWith id


--------------------------------------------------------------------------------
run :: FileSystem [ Switch ] -> IO ()
run =
  mapM_ $ mapM_ fromTemp <=< mapM toTemp
  where
    toTemp ( file1, file2 ) = do
      temp <- makeTempName 10
      renameFile file1 temp
      return ( temp, file2 )

    fromTemp ( temp, file2 ) =
      renameFile temp file2


--------------------------------------------------------------------------------
runPairwise :: FileSystem [ Switch ] -> IO ()
runPairwise =
  mapM_ $ mapM_ switcheroo
  where
    switcheroo ( file1, file2 ) = do
      temp <- makeTempName 10
      renameFile file1 temp
      renameFile file2 file1
      renameFile temp file2
