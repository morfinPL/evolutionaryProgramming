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


       putStrLn "Provide population size:"
       populationSizeString <- getLine
       let populationSize = read populationSizeString :: Int
       putStrLn "Provide number of features:"
       numberOfFeaturesString <- getLine
       let numberOfFeatures = read numberOfFeaturesString :: Int
       putStrLn "Provide number of iterations:"
       numberOfIterationsString <- getLine
       let numberOfIterations = read numberOfIterationsString :: Int


       let population = Utils.generatePopulation populationSize numberOfFeatures generator
       let computedPoints = Utils.computePoints objectiveFunction rangeX rangeY population
       Utils.plot "Initial population" objectiveFunctionString isoPoints groundLevel rangeX rangeY computedPoints
       let iterateFunction = Evolutionary.nextGeneration generator mutationProbability crossoverProbability rangeX rangeY objectiveFunction
       let newPopulation = iterate iterateFunction (population, computedPoints) !! (numberOfIterations -1)

       putStrLn "Solution:"
       print (head (Utils.lsort (snd newPopulation)))
       Utils.plot "New population" objectiveFunctionString isoPoints groundLevel rangeX rangeY (snd newPopulation)
