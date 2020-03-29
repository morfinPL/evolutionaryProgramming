module Evolutionary where

import qualified Data.List.Split                ( chunksOf )
import qualified System.Random                  ( RandomGen
                                                , randoms
                                                )

generatePopulation
  :: System.Random.RandomGen g
  => g
  -> Int
  -> Int
  -> Int
  -> ([Bool] -> [Bool])
  -> [[[Bool]]]
generatePopulation generator populationSize dimensions numberOfFeaturesPerDimension coding
  = map
    (map coding . Data.List.Split.chunksOf numberOfFeaturesPerDimension)
    (Data.List.Split.chunksOf
      (dimensions * numberOfFeaturesPerDimension)
      (take (dimensions * numberOfFeaturesPerDimension * populationSize)
            (System.Random.randoms generator :: [Bool])
      )
    )
  where booleaner x = x == 1

nextGeneration
  :: ([Bool] -> [Bool])
  -> ([[[Bool]]] -> [[Double]] -> [[[Bool]]])
  -> ([[[Bool]]] -> [[[Bool]]])
  -> ([[[Bool]]] -> [[[Bool]]])
  -> ([[[Bool]]] -> [[Double]])
  -> ([[[Bool]]], [[Double]])
  -> ([[[Bool]]], [[Double]])
nextGeneration decoding selection mutation crossover computePoints population =
  do
    let newPopulation = crossover (mutation (uncurry selection population))
    (newPopulation, computePoints newPopulation)
