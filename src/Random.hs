{- https://wiki.haskell.org/Random_shuffle -}

module Src.Random
  ( shuffleList
  ) where


--------------------------------------------------------------------------------
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           System.Random (RandomGen, getStdRandom, randomR)


--------------------------------------------------------------------------------
fisherYatesStep :: RandomGen g => ( Map Int a, g ) -> ( Int, a ) -> ( Map Int a, g )
fisherYatesStep ( m, gen ) ( i, x ) =
  ( Map.insert j x $ Map.insert i (m ! j) m, gen' )
  where
    ( j, gen' ) =
      randomR (0, i) gen


--------------------------------------------------------------------------------
fisherYates :: RandomGen g => g -> [ a ] -> ( [ a ], g )
fisherYates gen xs =
  case xs of
    [] ->
      ( [], gen )

    first:rest ->
      toElems $ foldl fisherYatesStep (initial first gen) (numerate rest)
      where
        toElems ( x, y ) = ( Map.elems x, y )
        numerate = zip [1..]
        initial x gen = ( Map.singleton 0 x, gen )


--------------------------------------------------------------------------------
shuffleList :: [ a ] -> IO [ a ]
shuffleList = getStdRandom . flip fisherYates
