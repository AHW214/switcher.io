module Util
  ( createUnique
  , filterTree
  , getTreeRoot
  , mapTreeRoot
  , partitionM
  , whenJust
  ) where


--------------------------------------------------------------------------------
import           Control.Monad (foldM, forM_)
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
