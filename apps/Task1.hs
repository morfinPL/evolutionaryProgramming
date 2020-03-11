module Task1 where

import qualified System.Random
import qualified Data.Ratio

import qualified Evolutionary
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
       Utils.plot "Initial population" Objectives.firstFunctionString 50 (-25) (-7.5, 7.5) (-7.5, 7.5) computedPoints
       let probabilities = Evolutionary.roulette computedPoints
       if populationSize <= 100
           then putStrLn ("Initial population:\n" ++ show computedPoints ++ "\n" ++ show probabilities)
       else putStrLn "Population is too big to be displayed."
       let newPopulation = Evolutionary.generateNewPopulationByRoulette generator population probabilities
       let newMutatedPopulation = Evolutionary.mutate generator (1 Data.Ratio.% 1000) numberOfFeatures newPopulation
       let newPopulationPoints = map (Utils.individualToPoint 2) newMutatedPopulation
       let newPopulationScaledXPoints = map (Utils.scale (-7.5, 7.5) 0) newPopulationPoints
       let newPopulationScaledPoints = map (Utils.scale (-7.5, 7.5) 1) newPopulationScaledXPoints
       let newComputedPoints = map (Utils.compute Objectives.firstFunction) newPopulationScaledPoints
       Utils.plot "New population" Objectives.firstFunctionString 50 (-25) (-7.5, 7.5) (-7.5, 7.5) newComputedPoints
       if populationSize <= 100
           then putStrLn ("New population: \n" ++ show newComputedPoints ++ "\n")
       else putStrLn "Population is too big to be displayed."
