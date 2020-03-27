module Disk
  ( findFilesWhere
  , findFilesWithExt
  , listFiles
  , listFileHierarchy
  ) where


--------------------------------------------------------------------------------
import           Control.Monad    (filterM)
import           Data.Tree        (Tree, unfoldTreeM)
import           System.Directory (doesFileExist, listDirectory, makeAbsolute,
                                   withCurrentDirectory)
import           System.FilePath  (FilePath, isExtensionOf, (</>))


--------------------------------------------------------------------------------
import           Util             (partitionM)


--------------------------------------------------------------------------------
listFilesAndDirs :: FilePath -> IO ( [ FilePath ], [ FilePath ] )
listFilesAndDirs dir =
    listDirectory dir >>= withCurrentDirectory dir . partitionM doesFileExist


--------------------------------------------------------------------------------
listFiles :: FilePath -> IO [ FilePath ]
listFiles dir =
  fst <$> listFilesAndDirs dir


--------------------------------------------------------------------------------
listFileHierarchy :: FilePath -> IO (Tree ( FilePath, [ FilePath ] ))
listFileHierarchy =
  unfoldTreeM makeNode
  where
    makeNode dir = do
      ( files, dirs ) <- listFilesAndDirs dir
      return ( ( dir, files ), map (dir </>) dirs )


--------------------------------------------------------------------------------
findFilesWhere :: (FilePath -> IO Bool) -> FilePath -> IO [ FilePath ]
findFilesWhere predicate dir =
  listFiles dir >>= filterM predicate


--------------------------------------------------------------------------------
findFilesWithExt :: String -> FilePath -> IO [ FilePath ]
findFilesWithExt ext =
  findFilesWhere (return . isExtensionOf ext)
