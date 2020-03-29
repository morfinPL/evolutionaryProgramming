module Evolutionary where

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
