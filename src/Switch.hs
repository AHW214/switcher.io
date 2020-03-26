module Switch
  ( generateSwitches
  , serializeSwitches
  , showSwitches
  , switch
  , switchPairwise
  ) where


--------------------------------------------------------------------------------
import           Data.List        (sort)
import           System.Directory (doesFileExist, renameFile)
import           System.FilePath  (FilePath)


--------------------------------------------------------------------------------
import           Random           (randomRSequence, shuffleList)


--------------------------------------------------------------------------------
type Switch =
  ( FilePath, FilePath )


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
  createUnique doesFileExist (\_ -> randomRSequence ( 'A', 'z' ) len)


--------------------------------------------------------------------------------
generateSwitches :: [ FilePath ] -> IO [ Switch ]
generateSwitches files =
  zip files <$> shuffleList files


--------------------------------------------------------------------------------
serializeSwitches :: [ Switch ] -> [ FilePath ] -> IO FilePath
serializeSwitches switches files = do
  mapName <- makeMapName
  writeFile mapName $ show switches
  return mapName
  where
    makeMapName =
      createUnique doesFileExist (\i -> return $ "switch" ++ show i)


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
