module Evolutionary where

import qualified Control.Monad.Random (evalRand, fromList, RandomGen)
import qualified Data.List (length, elemIndices, find)
import qualified Data.List.Split (chunksOf)
import qualified System.Random (StdGen, randoms, randomRs)

import qualified Utils (computePoints)


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
                    where computeSumOfProbabilities list = scanl1 (+) (init list)  ++ [1.0]


choicesToIndexes :: [Double] -> [Double] -> [Int]
choicesToIndexes choices roulette = map (choiceToIndex roulette) choices
    where choiceToIndex roulette a = head (Data.List.elemIndices (Data.List.find (>= a) roulette) (map Just roulette))


generateNewPopulationByRoulette :: System.Random.StdGen -> [[Bool]] -> [Double] -> [[Bool]]
generateNewPopulationByRoulette generator oldPopulation roulette = do
                                                         let choices = take (length oldPopulation) (System.Random.randoms generator :: [Double])
                                                         map (indexToIndividual oldPopulation) (choicesToIndexes choices roulette)
                                                            where indexToIndividual oldPopulation index = oldPopulation !! index


weightedList :: System.Random.StdGen -> [(a, Rational)] -> [a]
weightedList generator weights = Control.Monad.Random.evalRand m generator
                                 where m = sequence . repeat . Control.Monad.Random.fromList $ weights


mutate :: System.Random.StdGen -> Rational -> [[Bool]] -> [[Bool]]
mutate generator probability population = do
                                          let numberOfFeatures = length (head population)
                                          let flattenPopulation = concat population
                                          let operations = take (length flattenPopulation) (weightedList generator [(not, probability), (id, 1 -probability)])
                                          let tuples = zip operations flattenPopulation
                                          Data.List.Split.chunksOf numberOfFeatures (map apply tuples)
                                            where apply a = fst a (snd a)


cross :: Int -> [[Bool]] -> [[Bool]]
cross coordinate parents = do
                           let parentA = head parents
                           let parentB = last parents
                           [take coordinate parentA ++ drop coordinate parentB, take coordinate parentB ++ drop coordinate parentA]

crossover :: System.Random.StdGen -> Rational -> ([[Bool]], Int) -> [[Bool]]
crossover generator probability parents = if head (weightedList generator [(True, probability), (False, 1 -probability)])
                                            then
                                              cross (snd parents) (fst parents)
                                          else
                                            fst parents


crossoverPopulation :: System.Random.StdGen -> Rational -> [[Bool]] -> [[Bool]]
crossoverPopulation generator probability population = do
                                                       let pairedPopulation = Data.List.Split.chunksOf 2 population
                                                       let coordinates = take (length population) (System.Random.randomRs (1 :: Int, length (head population) -1) generator)
                                                       concatMap (crossover generator probability) (zip pairedPopulation coordinates)


nextGeneration :: System.Random.StdGen -> Rational -> Rational -> (Double, Double) -> (Double, Double) -> (Double -> Double -> Double) -> ([[Bool]], [[Double]]) -> ([[Bool]], [[Double]])
nextGeneration generator mutationProbability crossoverProbability rangeX rangeY objectiveFunction population = do
       let oldPopulation = fst population
       let oldPoints = snd population
       let probabilities = roulette oldPoints
       let parents = generateNewPopulationByRoulette generator oldPopulation probabilities
       let newPopulation = crossoverPopulation generator crossoverProbability (mutate generator mutationProbability parents)
       let newPoints = Utils.computePoints objectiveFunction rangeX rangeY newPopulation
       (newPopulation, newPoints)