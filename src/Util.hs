module Util
  ( FileName
  , atLeast
  , createUnique
  , directoryHasFile
  , getTreeRoot
  , mapTreeRoot
  , partitionM
  , pruneTree
  , renameInDirectory
  , whenJust
  ) where


--------------------------------------------------------------------------------
import           Control.Monad    (foldM, forM_)
import           Data.Maybe       (mapMaybe)
import           Data.Tree        (Tree(..))
import           System.Directory (doesFileExist, renameFile)
import           System.FilePath  ((</>))


--------------------------------------------------------------------------------
type FileName = String


--------------------------------------------------------------------------------
atLeast :: Int -> [ a ] -> Bool
atLeast n =
  if n > 0 then
    not . null . drop (n - 1)
  else
    const True


--------------------------------------------------------------------------------
partitionM :: (a -> IO Bool) -> [ a ] -> IO ( [ a ], [ a ] )
partitionM predicate =
  foldM update ( [], [] )
  where
    update ( passed, failed ) x = do
      res <- predicate x
      return $
        if res then
          ( x:passed, failed )
        else
          ( passed, x:failed )


--------------------------------------------------------------------------------
pruneTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
pruneTree predicate (Node value forest) =
  if predicate value || not (null below) then
    Just $ Node value below
  else
    Nothing
  where
    below =
      mapMaybe (pruneTree predicate) forest


--------------------------------------------------------------------------------
getTreeRoot :: Tree a -> a
getTreeRoot (Node value _) = value


--------------------------------------------------------------------------------
mapTreeRoot :: (a -> a) -> Tree a -> Tree a
mapTreeRoot f (Node value forest) =
  Node (f value) forest


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
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = forM_


--------------------------------------------------------------------------------
directoryHasFile :: FilePath -> FileName -> IO Bool
directoryHasFile dir =
  doesFileExist . (dir </>)


--------------------------------------------------------------------------------
renameInDirectory :: FilePath -> FileName -> FileName -> IO ()
renameInDirectory dir prev new =
  renameFile (dir </> prev) (dir </> new)
