module FileSystem
  ( FileSystem
  , build
  , buildWhere
  , buildWithExt
  , draw
  , drawMany
  , drawWith
  , drawManyWith
  , filter
  , mapFilter
  , mapLabels
  ) where


--------------------------------------------------------------------------------
import           Data.Bifunctor   (first)
import qualified Data.List        as List (filter)
import           Data.Tree        (Tree, drawTree, unfoldTreeM)
import           Prelude          hiding (filter)
import           System.Directory (doesFileExist, listDirectory)
import           System.FilePath  (FilePath, isExtensionOf, (</>))


--------------------------------------------------------------------------------
import           Util             (partitionM, filterTree)


--------------------------------------------------------------------------------
newtype FileSystem a =
  FileSystem (Tree ( FilePath, a ))
  deriving (Foldable, Functor, Read, Traversable, Show)


--------------------------------------------------------------------------------
draw :: (Show a) => FileSystem a -> String
draw = drawWith show


--------------------------------------------------------------------------------
drawMany :: (Show a) => FileSystem [ a ] -> String
drawMany = drawManyWith show


--------------------------------------------------------------------------------
drawWith :: (a -> String) -> FileSystem a -> String
drawWith toStr (FileSystem tree) =
  drawTree $ fmap showNode tree
  where
    showNode ( label, item ) =
      label ++ "\n" ++ toStr item


--------------------------------------------------------------------------------
drawManyWith :: (a -> String) -> FileSystem [ a ] -> String
drawManyWith toStr =
  drawWith (foldMap $ bullet . toStr)
  where
    bullet =
      ("-- " ++) . (++ "\n")


--------------------------------------------------------------------------------
mapLabels :: (FilePath -> FilePath) -> FileSystem a -> FileSystem a
mapLabels f (FileSystem tree) =
  FileSystem $ fmap (first f) tree


--------------------------------------------------------------------------------
mapFilter :: (a -> Bool) -> FileSystem [ a ] -> FileSystem [ a ]
mapFilter predicate =
  fmap $ List.filter predicate


--------------------------------------------------------------------------------
filter :: (a -> Bool) -> FileSystem a -> Maybe (FileSystem a)
filter predicate (FileSystem tree) =
  FileSystem <$> filterTree (predicate . snd) tree


--------------------------------------------------------------------------------
listFilesAndDirs :: FilePath -> IO ( [ FilePath ], [ FilePath ] )
listFilesAndDirs dir =
    listDirectory dir >>= partitionM doesFileExist . makeRelative
    where
      makeRelative =
        map (dir </>)


--------------------------------------------------------------------------------
build :: Int -> FilePath -> IO (FileSystem [ FilePath ])
build depth directory =
  FileSystem <$> unfoldTreeM makeNode ( depth, directory )
  where
    makeNode ( level, dir ) = do
      ( files, dirs ) <- listFilesAndDirs dir
      return ( ( dir, files ), subDirs level dir dirs )

    subDirs level dir =
      if level == 0 then
        const []
      else
        map ( level - 1, )


--------------------------------------------------------------------------------
buildWhere :: (FilePath -> Bool) -> Int -> FilePath
              -> IO (FileSystem [ FilePath ])
buildWhere predicate depth =
  fmap (mapFilter predicate) . build depth


--------------------------------------------------------------------------------
buildWithExt :: String -> Int -> FilePath -> IO (FileSystem [ FilePath ])
buildWithExt ext depth =
  buildWhere (isExtensionOf ext) depth
