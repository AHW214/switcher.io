module Switch
  ( Switch
  , generateSwitches
  , prepareUndo
  , readSwitches
  , serializeSwitches
  , showSwitches
  , switch
  , switchPairwise
  ) where


--------------------------------------------------------------------------------
import           Control.Monad    (join, (<=<))
import           Data.Bifunctor   (bimap)
import           Data.List        (sortBy)
import           Data.Maybe       (fromMaybe)
import           Data.Tuple       (swap)
import           System.Directory (doesFileExist, renameFile)
import           System.FilePath  (FilePath, takeFileName)
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
generateSwitches :: FileSystem [ FilePath ] -> IO (FileSystem [ Switch ])
generateSwitches =
  mapM switchFolder
  where
    switchFolder files =
      zip files <$> shuffleList files


--------------------------------------------------------------------------------
serializeSwitches :: FileSystem [ Switch ] -> FilePath -> IO FilePath
serializeSwitches switches dir = do
  mapName <- makeMapName
  writeFile mapName $ show switches
  return mapName
  where
    makeMapName =
      createUnique doesFileExist (return . ("switch" ++) . show)


--------------------------------------------------------------------------------
readSwitches :: FilePath -> IO (Maybe (FileSystem [ Switch ]))
readSwitches =
  fmap readMaybe . readFile


--------------------------------------------------------------------------------
prepareUndo :: [ Switch ] -> IO ( [ Switch ], [ Switch ] )
prepareUndo =
  partitionM canSwitch . invertSwitches
  where
    invertSwitches =
      map swap

    canSwitch =
      doesFileExist . fst


--------------------------------------------------------------------------------
showSwitchesInFolder :: [ Switch ] -> [ String ]
showSwitchesInFolder switches =
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
showSwitches :: FileSystem [ Switch ] -> String
showSwitches =
  FS.drawManyWith id . fmap showSwitchesInFolder


--------------------------------------------------------------------------------
switch :: FileSystem [ Switch ] -> IO ()
switch =
  mapM_ $ mapM_ fromTemp <=< mapM toTemp
  where
    toTemp ( file1, file2 ) = do
      temp <- makeTempName 10
      renameFile file1 temp
      return ( temp, file2 )

    fromTemp ( temp, file2 ) =
      renameFile temp file2


--------------------------------------------------------------------------------
switchPairwise :: FileSystem [ Switch ] -> IO ()
switchPairwise =
  mapM_ $ mapM_ switcheroo
  where
    switcheroo ( file1, file2 ) = do
      temp <- makeTempName 10
      renameFile file1 temp
      renameFile file2 file1
      renameFile temp file2
