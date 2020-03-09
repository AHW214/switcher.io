{- https://wiki.haskell.org/Random_shuffle -}

module Random
  ( randomSequence
  , randomRSequence
  , shuffleList
  ) where


--------------------------------------------------------------------------------
import           Data.Map      (Map, (!))
import qualified Data.Map      as Map
import           System.Random (Random, RandomGen, StdGen, getStdRandom,
                                random, randomR)


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


--------------------------------------------------------------------------------
randomSequenceHelper :: Random a => (StdGen -> ( a, StdGen )) -> Int -> IO [ a ]
randomSequenceHelper rand count =
  getStdRandom (\g -> helper ( [], g ) count)
  where
    helper acc@( seq, g ) n =
      if n <= 0 then
        acc
      else
        let ( r, g' ) = rand g in
        helper ( r:seq, g' ) (n - 1)


--------------------------------------------------------------------------------
randomRSequence :: Random a => ( a, a ) -> Int -> IO [ a ]
randomRSequence =
  randomSequenceHelper . randomR


--------------------------------------------------------------------------------
randomSequence :: Random a => Int -> IO [ a ]
randomSequence =
  randomSequenceHelper random
