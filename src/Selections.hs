module Selections where

import qualified Data.List                      ( length
                                                , elemIndices
                                                , find
                                                )
import qualified System.Random                  ( RandomGen
                                                , randoms
                                                )


computeProbabilities :: Double -> [Double] -> [Double]
computeProbabilities sum = map (divide sum) where divide sum elem = elem / sum


roulette :: [[Double]] -> [Double]
roulette points = do
  let values = map take3rdElement points where take3rdElement list = list !! 2
  let max          = maximum values
  let increasedMax = max + abs (0.1 * max)
  let differences = computeDifferences increasedMax values
        where computeDifferences max = map (subtract max)
  let sumOfDifferences = sum differences
  computeSumOfProbabilities (computeProbabilities sumOfDifferences differences)
  where computeSumOfProbabilities list = scanl1 (+) (init list) ++ [1.0]


choicesToIndexes :: [Double] -> [Double] -> [Int]
choicesToIndexes choices roulette = map (choiceToIndex roulette) choices
 where
  choiceToIndex roulette a = head
    (Data.List.elemIndices (Data.List.find (>= a) roulette) (map Just roulette))


rouletteSelection
  :: System.Random.RandomGen g => g -> [[[Bool]]] -> [[Double]] -> [[[Bool]]]
rouletteSelection generator oldPopulation oldPoints = do
  let rouletteArray = roulette oldPoints
  let choices = take (length oldPopulation)
                     (System.Random.randoms generator :: [Double])
  map (indexToIndividual oldPopulation) (choicesToIndexes choices rouletteArray)
  where indexToIndividual oldPopulation index = oldPopulation !! index
