module Evolutionary where

import qualified System.Random (StdGen, randoms)
import qualified Data.List (length, elemIndices, find)

take3rdElement :: [Double] -> Double
take3rdElement list = list !! 2

computeDifferences :: Double -> [Double] -> [Double]
computeDifferences max = map (subtract max)

divide :: Double -> Double -> Double
divide sum elem = elem / sum

computeProbabilities :: Double -> [Double] -> [Double]
computeProbabilities sum = map (divide sum)

sumToElem :: [Double] -> Int -> Double
sumToElem list a = sum (take a list)

computeSumOfProbabilities :: [Double] -> [Double]
computeSumOfProbabilities list = do
                                 let indexes = [1.. (length list -1)]
                                 let sums = map (sumToElem list) indexes
                                 sums ++ [1.0]
roulette :: [[Double]] -> [Double]
roulette points = do
                   let values = map take3rdElement points
                   let max = maximum values
                   let increasedMax = max + abs (0.1 * max)
                   let differences = computeDifferences increasedMax values
                   let sumOfDifferences = sum differences
                   computeSumOfProbabilities (computeProbabilities sumOfDifferences differences)

choiceToIndex :: [Double] -> Double -> Int
choiceToIndex roulette a = head (Data.List.elemIndices (Data.List.find (>= a) roulette) (map Just roulette))

choicesToIndexes :: [Double] -> [Double] -> [Int]
choicesToIndexes choices roulette = map (choiceToIndex roulette) choices

indexToIndividual :: [[Bool]] -> Int -> [Bool]
indexToIndividual oldPopulation index = oldPopulation !! index

generateNewPopulationByRoulette :: System.Random.StdGen -> [[Bool]] -> [Double] -> [[Bool]]
generateNewPopulationByRoulette generator oldPopulation roulette = do
                                                         let choices = take (length oldPopulation) (System.Random.randoms generator :: [Double])
                                                         map (indexToIndividual oldPopulation) (choicesToIndexes choices roulette)
