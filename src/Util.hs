module Util
  ( partitionM
  , pruneTree
  ) where


--------------------------------------------------------------------------------
import           Control.Monad (foldM)
import           Data.Maybe    (mapMaybe)
import           Data.Tree     (Tree(..))


--------------------------------------------------------------------------------
partitionM :: (a -> IO Bool) -> [ a ] -> IO ( [ a ], [ a ] )
partitionM predicate =
  foldM update ( [], [] )
  where
    update ( pass, fail ) x = do
      res <- predicate x
      return $
        if res then
          ( x:pass, fail )
        else
          ( pass, x:fail )


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
