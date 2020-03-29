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
    let oldPopulation = fst population
    let oldPoints     = snd population
    let parents       = selection oldPopulation oldPoints
    let newPopulation = crossover (mutation parents)
    let newPoints     = computePoints newPopulation
    (newPopulation, newPoints)
