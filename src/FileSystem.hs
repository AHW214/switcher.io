module FileSystem
  ( FileSystem
  , build
  , buildWhere
  , buildWithExt
  , draw
  , drawMany
  , drawWith
  , drawManyWith
  , getRootLabel
  , isEmpty
  , map
  , mapBoth
  , mapLabels
  , prune
  , runWith
  , runWith_
  , setRootLabel
  , unzip
  ) where


--------------------------------------------------------------------------------
import           Data.Bifunctor   (bimap, first)
import           Data.Monoid      (All(..))
import           Data.Tree        (Tree, drawTree, unfoldTreeM)
import           Prelude          hiding (map, unzip)
import           System.Directory (listDirectory)
import           System.FilePath  (isExtensionOf, (</>))


--------------------------------------------------------------------------------
import           Util             (FileName, directoryHasFile, getTreeRoot,
                                   mapTreeRoot, partitionM, pruneTree)


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
map :: (FilePath -> a -> b) -> FileSystem a -> FileSystem b
map f (FileSystem tree) =
  FileSystem $ fmap mapFolder tree
  where
    mapFolder ( dir, items ) =
      ( dir, f dir items )


--------------------------------------------------------------------------------
mapBoth :: (FilePath -> FilePath) -> (a -> b) -> FileSystem a -> FileSystem b
mapBoth f g (FileSystem tree) =
  FileSystem $ fmap (bimap f g) tree


--------------------------------------------------------------------------------
mapLabels :: (FilePath -> FilePath) -> FileSystem a -> FileSystem a
mapLabels f =
  mapBoth f id


--------------------------------------------------------------------------------
getRootLabel :: FileSystem a -> FilePath
getRootLabel (FileSystem tree) =
  fst $ getTreeRoot tree


--------------------------------------------------------------------------------
setRootLabel :: FilePath -> FileSystem a -> FileSystem a
setRootLabel label (FileSystem tree) =
  FileSystem $ mapTreeRoot (first $ const label) tree


--------------------------------------------------------------------------------
prune :: (a -> Bool) -> FileSystem a -> Maybe (FileSystem a)
prune predicate (FileSystem tree) =
  FileSystem <$> pruneTree (predicate . snd) tree


--------------------------------------------------------------------------------
unzip :: FileSystem ( a, b ) -> ( FileSystem a, FileSystem b )
unzip fs =
  ( fmap fst fs, fmap snd fs )


--------------------------------------------------------------------------------
isEmpty :: Foldable t => FileSystem (t a) -> Bool
isEmpty =
  getAll . foldMap (All . null)


--------------------------------------------------------------------------------
runWith :: Monad m => (FilePath -> a -> m b) -> FileSystem a -> m (FileSystem b)
runWith action (FileSystem tree) =
  FileSystem <$> mapM runFolder tree
  where
    runFolder ( dir, items ) =
      ( dir, ) <$> action dir items


--------------------------------------------------------------------------------
runWith_ :: Monad m => (FilePath -> a -> m b) -> FileSystem a -> m ()
runWith_ action (FileSystem tree) =
  mapM_ (uncurry action) tree


--------------------------------------------------------------------------------
listFilesAndDirs :: FilePath -> IO ( [ FileName ], [ FilePath ] )
listFilesAndDirs dir =
    listDirectory dir >>= partitionM (directoryHasFile dir)


--------------------------------------------------------------------------------
build :: Int -> FilePath -> IO (FileSystem [ FileName ])
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
        fmap $ ( level - 1, ) . (dir </>)


--------------------------------------------------------------------------------
buildWhere :: (FileName -> Bool) -> Int -> FilePath
              -> IO (FileSystem [ FileName ])
buildWhere predicate depth =
  fmap (fmap $ filter predicate) . build depth


--------------------------------------------------------------------------------
buildWithExt :: String -> Int -> FilePath -> IO (FileSystem [ FileName ])
buildWithExt ext =
  buildWhere (isExtensionOf ext)
