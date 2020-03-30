module Util
  ( filterTree
  , partitionM
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
filterTree :: (a -> Bool) -> Tree a -> Maybe (Tree a)
filterTree predicate (Node value forest) =
  if predicate value || not (null below) then
    Just $ Node value below
  else
    Nothing
  where
    below =
      mapMaybe (filterTree predicate) forest
