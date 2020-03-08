module Task1 where

import qualified System.Random

import qualified Objectives
import qualified Utils

main :: IO ()
main = do
       putStrLn "Provide needed population size:"
       populationSizeString <- getLine
       let populationSize = read populationSizeString :: Int
       putStrLn "Provide needed number of features:"
       numberOfFeaturesString <- getLine
       let numberOfFeatures = read numberOfFeaturesString :: Int
       generator <- System.Random.getStdGen
       let population = Utils.generatePopulation populationSize numberOfFeatures generator
       let populationPoints = map (Utils.individualToPoint 2) population
       let populationScaledXPoints = map (Utils.scale (-7.5, 7.5) 0) populationPoints
       let populationScaledPoints = map (Utils.scale (-7.5, 7.5) 1) populationScaledXPoints
       let computedPoints = map (Utils.compute Objectives.firstFunction) populationScaledPoints
       if populationSize * numberOfFeatures <= 100
           then putStrLn ("Initial population: " ++ show population ++ "\n" ++ show populationPoints ++ "\n" ++ show populationScaledXPoints ++ "\n" ++ show populationScaledPoints)
       else putStrLn "Initial population is too big to be displayed."
       Utils.plot Objectives.firstFunctionString 50 (-25) (-7.5, 7.5) (-7.5, 7.5) computedPoints