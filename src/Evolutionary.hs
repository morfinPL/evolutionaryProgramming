module Evolutionary where

import qualified Utils


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
