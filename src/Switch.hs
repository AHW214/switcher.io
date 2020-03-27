module Switch
  ( Switch
  , SwitchMap(..)
  , generateSwitches
  , generateSwitchesRecursive
  , prepareUndo
  , readSwitches
  , serializeSwitches
  , showSwitches
  , showSwitchesRecursive
  , switch
  , switchPairwise
  ) where


--------------------------------------------------------------------------------
import           Data.List        (sort)
import           Data.Maybe       (fromMaybe)
import           Data.Tree        (Tree, drawTree)
import           Data.Tuple       (swap)
import           System.Directory (doesFileExist, renameFile)
import           System.FilePath  (FilePath)
import           Text.Read        (readMaybe)


--------------------------------------------------------------------------------
import           Disk             (Hierarchy)
import           Random           (randomRSequence, shuffleList)
import           Util             (partitionM)


--------------------------------------------------------------------------------
type Switch =
  ( FilePath, FilePath )


--------------------------------------------------------------------------------
data SwitchMap
  = SwitchMap
    { directory :: FilePath
    , switches :: [ Switch ]
    } deriving (Read, Show)


--------------------------------------------------------------------------------
createUnique :: (a -> IO Bool) -> (Int -> IO a) -> IO a
createUnique exists gen =
  attempt 0
  where
    attempt i = do
      x <- gen i
      conflict <- exists x

      if conflict then
        attempt (i + 1)
      else
        return x


--------------------------------------------------------------------------------
makeTempName :: Int -> IO FilePath
makeTempName len =
  createUnique doesFileExist (const $ randomRSequence ( 'A', 'z' ) len)


--------------------------------------------------------------------------------
generateSwitches :: [ FilePath ] -> IO [ Switch ]
generateSwitches files =
  zip files <$> shuffleList files


--------------------------------------------------------------------------------
generateSwitchesRecursive :: Hierarchy -> IO (Tree ( FilePath, [ Switch ] ))
generateSwitchesRecursive =
  mapM (\( dir, files ) -> (,) dir <$> generateSwitches files)


--------------------------------------------------------------------------------
serializeSwitches :: [ Switch ] -> FilePath -> IO FilePath
serializeSwitches switches dir = do
  mapName <- makeMapName
  writeFile mapName $ show switchMap
  return mapName
  where
    makeMapName =
      createUnique doesFileExist (\i -> return $ "switch" ++ show i)

    switchMap =
      SwitchMap dir switches


--------------------------------------------------------------------------------
readSwitches :: FilePath -> IO (Maybe SwitchMap)
readSwitches switchFile = do
  readMaybe <$> readFile switchFile


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
showSwitches :: [ Switch ] -> String
showSwitches switches =
  concatMap showSwitch sorted
  where
    sorted =
      sort switches

    padLen =
      maximum $ map (length . fst) switches

    padding file =
      replicate (padLen - length file) ' '

    showSwitch ( file1, file2 ) =
      file1 ++ padding file1 ++ " -> " ++ file2 ++ "\n"


--------------------------------------------------------------------------------
showSwitchesRecursive :: Tree ( FilePath, [ Switch ] ) -> String
showSwitchesRecursive tree =
  drawTree $ showFolder <$> tree
  where
    showFolder ( dir, switches ) =
      dir ++ "\n" ++ showSwitches switches


--------------------------------------------------------------------------------
switch :: [ Switch ] -> IO ()
switch switches =
  mapM toTemp switches >>= mapM_ fromTemp
  where
    toTemp ( file1, file2 ) = do
      temp <- makeTempName 10
      renameFile file1 temp
      return ( temp, file2 )

    fromTemp ( temp, file2 ) =
      renameFile temp file2


--------------------------------------------------------------------------------
switchPairwise :: [ Switch ] -> IO ()
switchPairwise =
  mapM_ switcheroo
  where
    switcheroo ( file1, file2 ) = do
      temp <- makeTempName 10
      renameFile file1 temp
      renameFile file2 file1
      renameFile temp file2
