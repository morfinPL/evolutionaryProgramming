module Task1 where

import qualified System.Random
import qualified Data.Ratio

import qualified Evolutionary
import qualified Objectives
import qualified Utils


main :: IO ()
main = do
       let objectiveFunction = Objectives.firstFunction
       let objectiveFunctionString = Objectives.firstFunctionString
       let rangeX = (-7.5, 7.5)
       let rangeY = (-7.5, 7.5)
       let isoPoints = 50
       let groundLevel = -25
       let mutationProbability = 1 Data.Ratio.% 1000
       let crossoverProbability = 6 Data.Ratio.% 10
       generator <- System.Random.getStdGen


       putStrLn "Provide needed population size:"
       populationSizeString <- getLine
       let populationSize = read populationSizeString :: Int
       putStrLn "Provide needed number of features:"
       numberOfFeaturesString <- getLine
       let numberOfFeatures = read numberOfFeaturesString :: Int


       let population = Utils.generatePopulation populationSize numberOfFeatures generator
       let computedPoints = Utils.computePoints objectiveFunction rangeX rangeY population
       Utils.plot "Initial population" objectiveFunctionString isoPoints groundLevel rangeX rangeY computedPoints
       let newPopulation = Evolutionary.nextGeneration generator mutationProbability crossoverProbability population computedPoints
       let newComputedPoints = Utils.computePoints objectiveFunction rangeX rangeY newPopulation
       Utils.plot "New population" objectiveFunctionString isoPoints groundLevel rangeX rangeY newComputedPoints
