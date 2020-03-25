module Switch
  ( generateSwitches
  , serializeSwitches
  , switch
  , switchPairwise
  ) where


--------------------------------------------------------------------------------
import           System.Directory (doesFileExist, renameFile)
import           System.FilePath  (FilePath)


--------------------------------------------------------------------------------
import           Random           (randomRSequence, shuffleList)


--------------------------------------------------------------------------------
type Switch =
  ( FilePath, FilePath, FilePath )


--------------------------------------------------------------------------------
createUnique :: Eq a => [ a ] -> (Int -> IO a) -> IO a
createUnique exclude gen =
  attempt 0
  where
    attempt i = do
      x <- gen i
      if x `elem` exclude then
        attempt (i + 1)
      else
        return x


--------------------------------------------------------------------------------
makeTempName :: Int -> [ FilePath ] -> IO FilePath
makeTempName len =
  flip createUnique (\_ -> randomRSequence ( 'A', 'z' ) len)


--------------------------------------------------------------------------------
generateSwitches :: [ FilePath ] -> IO [ Switch ]
generateSwitches files = do
  shuffled <- shuffleList files
  temps <- mapM (\_ -> makeTempName 10 files) files
  return $ zip3 files temps shuffled


--------------------------------------------------------------------------------
serializeSwitches :: [ Switch ] -> [ FilePath ] -> IO ()
serializeSwitches switches files = do
  mapName <- makeMapName
  writeFile mapName $ show switchMap
  where
    makeMapName =
      createUnique files (\i -> return $ "switchMap" ++ show i)

    switchMap =
      map (\( file1, _, file2 ) -> ( file1, file2 )) switches


--------------------------------------------------------------------------------
switch :: [ Switch ] -> IO ()
switch map =
  mapM_ toTemp map >> mapM_ fromTemp map
  where
    toTemp ( file1, temp, _ ) =
      renameFile file1 temp

    fromTemp ( _, temp, file2 ) =
      renameFile temp file2


--------------------------------------------------------------------------------
switchPairwise :: [ Switch ] -> IO ()
switchPairwise =
  mapM_ switcheroo
  where
    switcheroo ( file1, temp, file2 ) =
      renameFile file1 temp
      >> renameFile file2 file1
      >> renameFile temp file2
