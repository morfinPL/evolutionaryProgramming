module Task1 where

import qualified System.Random
import qualified Data.List

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
       if populationSize * numberOfFeatures <= 100
           then putStrLn ("Initial population: " ++ show population)
       else putStrLn "Initial population is too big to be displayed."
       Utils.plot Objectives.firstFunctionString 50 (-25) (-7.5) 7.5 (-7.5) 7.5 (-1) 0 (-25)