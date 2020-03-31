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
  , getRootLabel
  , isEmpty
  , mapFilter
  , mapLabels
  , setRootLabel
  , unzip
  ) where


--------------------------------------------------------------------------------
import           Data.Bifunctor   (first)
import qualified Data.List        as List (filter)
import           Data.Monoid      (All(..))
import           Data.Tree        (Tree, drawTree, unfoldTreeM)
import           Prelude          hiding (filter, unzip)
import           System.Directory (doesFileExist, listDirectory)
import           System.FilePath  (FilePath, isExtensionOf, (</>))


--------------------------------------------------------------------------------
import           Util             (filterTree, getTreeRoot, mapTreeRoot,
                                   partitionM)


--------------------------------------------------------------------------------
newtype FileSystem a =
  FileSystem (Tree ( FilePath, a ))
  deriving (Foldable, Functor, Read, Traversable, Show)


--------------------------------------------------------------------------------
draw :: Show a => FileSystem a -> String
draw = drawWith show


--------------------------------------------------------------------------------
drawMany :: Show a => FileSystem [ a ] -> String
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
getRootLabel :: FileSystem a -> FilePath
getRootLabel (FileSystem tree) =
  fst $ getTreeRoot tree


--------------------------------------------------------------------------------
setRootLabel :: FilePath -> FileSystem a -> FileSystem a
setRootLabel label (FileSystem tree) =
  FileSystem $ mapTreeRoot (first $ const label) tree


--------------------------------------------------------------------------------
mapFilter :: (a -> Bool) -> FileSystem [ a ] -> FileSystem [ a ]
mapFilter predicate =
  fmap $ List.filter predicate


--------------------------------------------------------------------------------
filter :: (a -> Bool) -> FileSystem a -> Maybe (FileSystem a) -- change from maybe to 'empty' tree
filter predicate (FileSystem tree) =
  FileSystem <$> filterTree (predicate . snd) tree


--------------------------------------------------------------------------------
unzip :: FileSystem ( a, b ) -> ( FileSystem a, FileSystem b )
unzip fs =
  ( fmap fst fs, fmap snd fs )


--------------------------------------------------------------------------------
isEmpty :: Traversable t => FileSystem (t a) -> Bool
isEmpty =
  getAll . foldMap (All . null)


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
buildWithExt ext =
  buildWhere (isExtensionOf ext)
