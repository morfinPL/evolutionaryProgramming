{-# LANGUAGE OverloadedStrings #-}

module Task1 where

import qualified System.Random
import qualified Data.Text (pack)
import qualified Data.Ini.Config (IniParser, section, fieldOf, number, parseIniFile)
import Data.Either (fromRight)
import qualified Data.Ratio
import Control.Exception
import qualified Formatting
import Formatting.Clock
import System.Clock

import qualified Evolutionary
import qualified Objectives
import qualified Utils


main :: IO ()
main = do
       config <- getConfig
       let objectiveFunction = Objectives.firstFunction
       let objectiveFunctionString = Objectives.firstFunctionString
       let rangeX = (-7.5, 7.5)
       let rangeY = (-7.5, 7.5)
       let isoPoints = 50
       let groundLevel = -25
       generator <- System.Random.getStdGen

       let population = Utils.generatePopulation (populationSize config) (features config) generator
       let computedPoints = Utils.computePoints objectiveFunction rangeX rangeY population
       putStrLn "Best initial guess:"
       print (head (Utils.sortByObjectiveFunctionValue computedPoints))
       Utils.plot "Initial population" objectiveFunctionString isoPoints groundLevel rangeX rangeY computedPoints
       start <- getTime Monotonic
       let iterateFunction = Evolutionary.nextGeneration generator (mutationProbability config) (crossoverProbability config) rangeX rangeY objectiveFunction
       let newPopulation = iterate iterateFunction (population, computedPoints) !! (iterations config -1)

       putStrLn "Solution:"
       print (head (Utils.sortByObjectiveFunctionValue (snd newPopulation)))
       end <- getTime Monotonic
       putStrLn "Processing time:"
       Formatting.fprint timeSpecs start end
       Utils.plot "Final population" objectiveFunctionString isoPoints groundLevel rangeX rangeY (snd newPopulation)


data Config = Config
  { populationSize :: Int
  , features :: Int
  , iterations :: Int
  , mutationProbability :: Rational
  , crossoverProbability :: Rational
  } deriving (Eq, Show)


parseConfig :: Data.Ini.Config.IniParser Config
parseConfig = Data.Ini.Config.section "Task1" $ do
  populationSize <- Data.Ini.Config.fieldOf "populationSize" Data.Ini.Config.number
  features <- Data.Ini.Config.fieldOf "features" Data.Ini.Config.number
  iterations <- Data.Ini.Config.fieldOf "iterations" Data.Ini.Config.number
  mutationProbability <- Data.Ini.Config.fieldOf "mutationProbability" Data.Ini.Config.number
  crossoverProbability <- Data.Ini.Config.fieldOf "crossoverProbability" Data.Ini.Config.number
  return (Config populationSize features iterations mutationProbability crossoverProbability)

getConfig = do
            configFile <- readFile "config\\Task1\\config.txt"
            let parsingResult = Data.Ini.Config.parseIniFile (Data.Text.pack configFile) parseConfig
            case parsingResult of
                Left message -> do
                                putStrLn message
                                putStrLn "Default config will be used!"
                Right config -> putStrLn "Config successfully loaded!"
            let defaultConfig = Config 1024 64 1000 (1 Data.Ratio.% 1000) (6 Data.Ratio.% 10)
            let config = fromRight defaultConfig parsingResult
            putStrLn "Config:"
            print config
            return config
