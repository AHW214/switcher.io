module Switch
  ( Switches
  , displayEach
  , generate
  , isEmpty
  , load
  , sanitize
  , serialize
  , switch
  ) where


--------------------------------------------------------------------------------
import           Control.Exception (tryJust)
import           Control.Monad     (guard)
import           Data.List         (sortBy)
import           Data.Ord          (comparing)
import           System.Directory  (doesFileExist)
import           System.IO.Error   (isDoesNotExistError)
import           Text.Read         (readMaybe)


--------------------------------------------------------------------------------
import           Random           (randomRSequence, shuffleList)
import           Util             (FileName, both, createUnique,
                                   directoryHasFile, pairs, partitionSequenceM,
                                   renameInDirectory, wrap)


--------------------------------------------------------------------------------
newtype Switches =
  Switches [ [ FileName ] ]
  deriving (Read, Show)


--------------------------------------------------------------------------------
makeTempName :: FilePath -> Int -> IO FileName
makeTempName dir len =
  createUnique (directoryHasFile dir) (const $ randomRSequence ( 'A', 'Z' ) len)


--------------------------------------------------------------------------------
isEmpty :: Switches -> Bool
isEmpty (Switches seqs) =
  null $ concat seqs


--------------------------------------------------------------------------------
generate :: [ FileName ] -> IO Switches
generate =
  fmap (Switches . return . wrap) . shuffleList


--------------------------------------------------------------------------------
serialize :: Show (t Switches) => t Switches -> IO FilePath
serialize switches = do
  fileName <- makeFileName
  writeFile fileName $ show switches
  return fileName
  where
    makeFileName =
      createUnique doesFileExist $ return . ("switch" ++) . show


--------------------------------------------------------------------------------
load :: Read (t Switches) => FilePath -> IO (Either String (t Switches))
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
sanitize :: FilePath -> Switches -> IO ( Switches, Switches )
sanitize dir (Switches seqs) =
  both (Switches . concat) . unzip <$> mapM (partitionSequenceM canSwitch . reverse) seqs
  where
    canSwitch =
      directoryHasFile dir


--------------------------------------------------------------------------------
displayEach :: Switches -> [ String ]
displayEach (Switches seqs) =
  map showSwitch sorted
  where
    paired =
      concatMap pairs seqs

    sorted =
      sortBy (comparing fst) paired

    padLen =
      maximum $ map (length . fst) paired

    padding file =
      replicate (padLen - length file) ' '

    showSwitch ( file1, file2 ) =
      file1 ++ padding file1 ++ " -> " ++ file2


--------------------------------------------------------------------------------
switch :: FilePath -> Switches -> IO ()
switch dir (Switches seqs) =
  mapM_ (switchSequence dir) seqs


--------------------------------------------------------------------------------
switchSequence :: FilePath -> [ FileName ] -> IO ()
switchSequence dir files = do
  t1 <- makeTemp
  t2 <- makeTemp
  switchSeq t1 t2 files
  where
    makeTemp =
      makeTempName dir 10

    switchSeq t1 t2 =
      \case
        f1:f2:[] ->
          rename f1 f2

        f1:f2:fs -> do
          rename f2 t1
          rename f1 f2
          switchSeq t2 t1 (t1:fs)

        _ ->
          return ()

    rename =
      renameInDirectory dir
