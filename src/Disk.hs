module Disk
  ( Hierarchy
  , findFilesWhere
  , findFilesWhereRecursive
  , findFilesWithExt
  , findFilesWithExtRecursive
  , listFiles
  , viewFileHierarchy
  , viewFilesRecursive
  ) where


--------------------------------------------------------------------------------
import           Control.Monad    (filterM)
import           Data.Bifunctor   (second)
import           Data.Maybe       (mapMaybe)
import           Data.Tree        (Tree(..), unfoldTreeM)
import           System.Directory (doesFileExist, listDirectory, makeAbsolute,
                                   withCurrentDirectory)
import           System.FilePath  (FilePath, isExtensionOf, (</>))


--------------------------------------------------------------------------------
import           Util             (partitionM)


--------------------------------------------------------------------------------
type Hierarchy =
  Tree ( FilePath, [ FilePath ] )


--------------------------------------------------------------------------------
pruneTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
pruneTree isNodeEmpty tree@(Node value forest) =
  if isNodeEmpty value && null below then
    Nothing
  else
    Just $ Node value below
  where
    below =
      mapMaybe (pruneTree isNodeEmpty) forest


--------------------------------------------------------------------------------
listFilesAndDirs :: FilePath -> IO ( [ FilePath ], [ FilePath ] )
listFilesAndDirs dir =
    listDirectory dir >>= withCurrentDirectory dir . partitionM doesFileExist


--------------------------------------------------------------------------------
listFiles :: FilePath -> IO [ FilePath ]
listFiles dir =
  fst <$> listFilesAndDirs dir


--------------------------------------------------------------------------------
viewFileHierarchy :: Int -> FilePath -> IO Hierarchy
viewFileHierarchy depth directory =
  unfoldTreeM makeNode ( directory, depth )
  where
    makeNode ( dir, level ) = do
      ( files, dirs ) <- listFilesAndDirs dir
      return ( ( dir, files ), nextLevel dir level dirs )

    nextLevel dir level =
      if level == 0 then
        const []
      else
        map (\d -> ( dir </> d, level - 1))


--------------------------------------------------------------------------------
viewFilesRecursive :: Int -> FilePath -> IO (Maybe Hierarchy)
viewFilesRecursive depth dir =
  pruneTree (null . snd) <$> viewFileHierarchy depth dir


--------------------------------------------------------------------------------
findFilesWhereRecursive :: (FilePath -> Bool) -> Int -> FilePath -> IO (Maybe Hierarchy)
findFilesWhereRecursive predicate depth dir =
  pruneTree (null . snd) . fmap (second $ filter predicate) <$> viewFileHierarchy depth dir


--------------------------------------------------------------------------------
findFilesWithExtRecursive :: String -> Int -> FilePath -> IO (Maybe Hierarchy)
findFilesWithExtRecursive ext =
  findFilesWhereRecursive (isExtensionOf ext)


--------------------------------------------------------------------------------
findFilesWhere :: (FilePath -> IO Bool) -> FilePath -> IO [ FilePath ]
findFilesWhere predicate dir =
  listFiles dir >>= filterM predicate


--------------------------------------------------------------------------------
findFilesWithExt :: String -> FilePath -> IO [ FilePath ]
findFilesWithExt ext =
  findFilesWhere (return . isExtensionOf ext)
