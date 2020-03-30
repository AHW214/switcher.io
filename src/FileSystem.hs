module FileSystem
  ( FileSystem
  , buildFileSystem
  , drawFileSystem
  , findFilesWhere
  , findFilesWithExt
  , listFiles
  , listFilesAndDirs
  ) where


--------------------------------------------------------------------------------
import           Control.Monad    (filterM, (<=<))
import           Data.Tree        (Tree, drawTree, unfoldTreeM)
import           System.Directory (doesFileExist, listDirectory)
import           System.FilePath  (FilePath, isExtensionOf, (</>))


--------------------------------------------------------------------------------
import           Util             (partitionM, pruneTree)


--------------------------------------------------------------------------------
newtype FileSystem a =
  FileSystem (Tree ( FilePath, a ))
  deriving (Foldable, Functor, Traversable, Show)


--------------------------------------------------------------------------------
listFilesAndDirs :: FilePath -> IO ( [ FilePath ], [ FilePath ] )
listFilesAndDirs dir =
    listDirectory dir >>= partitionM isFile
    where
      isFile =
        doesFileExist . (dir </>)


--------------------------------------------------------------------------------
buildFileSystem :: Int -> FilePath -> IO (FileSystem [ FilePath ])
buildFileSystem depth directory =
  FileSystem <$> unfoldTreeM makeNode ( depth, directory )
  where
    makeNode ( level, dir ) = do
      ( files, dirs ) <- listFilesAndDirs dir
      return ( ( dir, files ), subDirs level dir dirs )

    subDirs level dir =
      if level == 0 then
        const []
      else
        map (\d -> ( level - 1, dir </> d ))


--------------------------------------------------------------------------------
drawFileSystem :: (Show a) => FileSystem a -> String
drawFileSystem (FileSystem tree) =
  drawTree $ fmap show tree


--------------------------------------------------------------------------------
listFiles :: FilePath -> IO [ FilePath ]
listFiles dir =
  fst <$> listFilesAndDirs dir


--------------------------------------------------------------------------------
findFilesWhere :: (FilePath -> IO Bool) -> FilePath -> IO [ FilePath ]
findFilesWhere predicate dir =
  listFiles dir >>= filterM predicate


--------------------------------------------------------------------------------
findFilesWithExt :: String -> FilePath -> IO [ FilePath ]
findFilesWithExt ext =
  findFilesWhere (return . isExtensionOf ext)
