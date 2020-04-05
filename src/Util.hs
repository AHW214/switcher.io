module Util
  ( FileName
  , atLeast
  , both
  , createUnique
  , directoryHasFile
  , getTreeRoot
  , mapTreeRoot
  , pairs
  , partitionM
  , partitionSequenceM
  , pruneTree
  , renameInDirectory
  , splitAfterM
  , whenJust
  , wrap
  ) where


--------------------------------------------------------------------------------
import           Control.Monad    (foldM, forM_)
import           Data.Functor     ((<&>))
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
both :: (a -> b) -> ( a, a ) -> ( b, b )
both f ( x, y ) = ( f x, f y )


--------------------------------------------------------------------------------
wrap :: [ a ] -> [ a ]
wrap xs =
  case xs of
    x:_ ->
      xs ++ [ x ]

    _ ->
      []


--------------------------------------------------------------------------------
pairs :: [ a ] -> [ ( a, a ) ]
pairs =
  \case
    x1:x2:xs ->
      ( x1, x2 ) : pairs (x2:xs)

    _ ->
      []


--------------------------------------------------------------------------------
{- http://hackage.haskell.org/package/list-grouping-0.1.1/docs/src/Data-List-Grouping.html -}

splitAfterM :: Monad m => (a -> m Bool) -> [ a ] -> m [ [ a ] ]
splitAfterM _ [] = return []
splitAfterM p xss@(x:xs) =
  p x >>=
    \case
      True ->
        ([ x ] :) <$> splitAfterM p xs

      _ ->
        splitAfterM p xs <&>
          \case
            [] ->
              [ xss ]

            ys:zs ->
              (x:ys) : zs


--------------------------------------------------------------------------------
partitionSequenceM ::
  Monad m => (a -> m Bool) -> [ a ] -> m ( [ [ a ] ], [ [ a ] ] )
partitionSequenceM predicate xs = do
  keep <- remove (fmap not . predicate) xs
  discard <- remove predicate xs

  return ( keep, discard )
  where
    remove p =
      fmap (filter $ atLeast 2) . splitAfterM p


--------------------------------------------------------------------------------
partitionM :: Monad m => (a -> m Bool) -> [ a ] -> m ( [ a ], [ a ] )
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
