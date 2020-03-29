module Evolutionary where

import qualified Data.List                      ( length )
import qualified Data.List.Split                ( chunksOf )
import qualified System.Random                  ( RandomGen
                                                , randomRs
                                                )

import qualified Utils


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
  let
    crossoverIndicators = concatMap
      (replicate dimensions)
      (take
        (length pairs)
        (Utils.weightedList generator
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
