module Evolutionary where

import qualified Control.Monad.Random (evalRand, fromList, RandomGen)
import qualified Data.List (length, elemIndices, find)
import qualified Data.List.Split (chunksOf)
import qualified System.Random (StdGen, randoms)


computeProbabilities :: Double -> [Double] -> [Double]
computeProbabilities sum = map (divide sum)
    where divide sum elem = elem / sum


roulette :: [[Double]] -> [Double]
roulette points = do
                  let values = map take3rdElement points
                               where take3rdElement list = list !! 2
                  let max = maximum values
                  let increasedMax = max + abs (0.1 * max)
                  let differences = computeDifferences increasedMax values
                                    where computeDifferences max = map (subtract max)
                  let sumOfDifferences = sum differences
                  computeSumOfProbabilities (computeProbabilities sumOfDifferences differences)
                    where computeSumOfProbabilities list = do
                                 let indexes = [1.. (length list -1)]
                                 let sums = map (sumToElem list) indexes
                                            where sumToElem list a = sum (take a list)
                                 sums ++ [1.0]


choicesToIndexes :: [Double] -> [Double] -> [Int]
choicesToIndexes choices roulette = map (choiceToIndex roulette) choices
    where choiceToIndex roulette a = head (Data.List.elemIndices (Data.List.find (>= a) roulette) (map Just roulette))


generateNewPopulationByRoulette :: System.Random.StdGen -> [[Bool]] -> [Double] -> [[Bool]]
generateNewPopulationByRoulette generator oldPopulation roulette = do
                                                         let choices = take (length oldPopulation) (System.Random.randoms generator :: [Double])
                                                         map (indexToIndividual oldPopulation) (choicesToIndexes choices roulette)
                                                            where indexToIndividual oldPopulation index = oldPopulation !! index


mutate :: System.Random.StdGen -> Rational -> [[Bool]] -> [[Bool]]
mutate generator probability population = do
                                          let numberOfFeatures = length (head population)
                                          let flattenPopulation = concat population
                                          let operations = take (length flattenPopulation) (weightedList generator [(not, probability), (id, 1 -probability)])
                                                            where weightedList generator weights = Control.Monad.Random.evalRand m generator
                                                                                                    where m = sequence . repeat . Control.Monad.Random.fromList $ weights
                                          let tuples = zip operations flattenPopulation
                                          Data.List.Split.chunksOf numberOfFeatures (map apply tuples)
                                            where apply a = fst a (snd a)


nextGeneration :: System.Random.StdGen -> Rational -> [[Bool]] -> [[Double]] -> [[Bool]]
nextGeneration generator mutationProbability population computedPoints = do
       let probabilities = roulette computedPoints
       let newPopulation = generateNewPopulationByRoulette generator population probabilities
       mutate generator mutationProbability newPopulation
