module Evolutionary where

import qualified Control.Monad.Random           ( evalRand
                                                , fromList
                                                )
import qualified Data.List                      ( length )
import qualified Data.List.Split                ( chunksOf )
import qualified System.Random                  ( RandomGen
                                                , randomRs
                                                )

import qualified Utils                          ( computePoints )


weightedList :: System.Random.RandomGen g => g -> [(a, Rational)] -> [a]
weightedList generator weights = Control.Monad.Random.evalRand m generator
  where m = sequence . repeat . Control.Monad.Random.fromList $ weights


flipBitMutation
  :: System.Random.RandomGen g => g -> Rational -> [[[Bool]]] -> [[[Bool]]]
flipBitMutation generator probability population = do
  let dimensions               = length (head population)
  let numberOfBitsPerDimension = length (head (head population))
  let flattenPopulation        = concat (concat population)
  let operations = take
        (length flattenPopulation)
        (weightedList generator [(not, probability), (id, 1 - probability)])
  let tuples = zip operations flattenPopulation
  Data.List.Split.chunksOf
    dimensions
    (Data.List.Split.chunksOf numberOfBitsPerDimension (map apply tuples))
  where apply a = fst a (snd a)


onePointCrossPair :: (([Bool], [Bool]), Int) -> ([Bool], [Bool])
onePointCrossPair tuple = do
  let coordinate = snd tuple
  let parents    = fst tuple
  let parentA    = fst parents
  let parentB    = snd parents
  let childA = take coordinate parentA ++ drop coordinate parentB
  let childB = take coordinate parentB ++ drop coordinate parentA
  (childA, childB)

coordinatesToIndividuals :: [([Bool], [Bool])] -> [[[Bool]]]
coordinatesToIndividuals pairCoordinates = do
  let childA = map fst pairCoordinates
  let childB = map snd pairCoordinates
  [childA, childB]

onePointCrossover
  :: System.Random.RandomGen g => g -> Rational -> [[[Bool]]] -> [[[Bool]]]
onePointCrossover generator probability population = do
  let dimensions         = length (head population)
  let pairs              = Data.List.Split.chunksOf 2 population
  let pairsOfCoordinates = map (uncurry zip . (\x -> (head x, last x))) pairs
  let crossoverIndicators = concatMap
        (replicate dimensions)
        (take
          (length pairs)
          (weightedList generator
                        [(True, probability), (False, 1 - probability)]
          )
        )
  let pointsOfCrossover = take
        (length pairs * dimensions)
        (System.Random.randomRs
          (1 :: Int, length (head (head population)) - 1)
          generator
        )
  let coordinatesWithPointsOfCrossoverAndIndicators = zip
        (zip (concat pairsOfCoordinates) pointsOfCrossover)
        crossoverIndicators
  let crossedCoordinates = map crossoverIf
                               coordinatesWithPointsOfCrossoverAndIndicators       where
        crossoverIf bigTuple = if snd bigTuple
          then onePointCrossPair (fst bigTuple)
          else fst (fst bigTuple)
  concatMap coordinatesToIndividuals
            (Data.List.Split.chunksOf dimensions crossedCoordinates)


nextGeneration
  :: ([Bool] -> [Bool])
  -> ([[[Bool]]] -> [[Double]] -> [[[Bool]]])
  -> ([[[Bool]]] -> [[[Bool]]])
  -> ([[[Bool]]] -> [[[Bool]]])
  -> (Double, Double)
  -> (Double, Double)
  -> (Double -> Double -> Double)
  -> ([[[Bool]]], [[Double]])
  -> ([[[Bool]]], [[Double]])
nextGeneration decoding selection mutation crossover rangeX rangeY objectiveFunction population
  = do
    let oldPopulation = fst population
    let oldPoints     = snd population
    let parents       = selection oldPopulation oldPoints
    let newPopulation = crossover (mutation parents)
    let newPoints = Utils.computePoints objectiveFunction
                                        rangeX
                                        rangeY
                                        decoding
                                        newPopulation
    (newPopulation, newPoints)
