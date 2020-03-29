{-# LANGUAGE OverloadedStrings #-}

module Task1 where

import qualified System.Environment             ( getArgs )
import qualified Random.Xorshift.Int64          ( newXorshift64 )
import qualified System.Directory               ( doesDirectoryExist
                                                , removeDirectoryRecursive
                                                )
import qualified Control.Monad                  ( mapM_
                                                , when
                                                )
import qualified Data.Text                      ( pack )
import qualified Data.Ini.Config                ( IniParser
                                                , section
                                                , fieldOf
                                                , number
                                                , string
                                                , parseIniFile
                                                )
import qualified Data.Either                    ( fromRight )
import qualified Data.List                      ( length )
import qualified Data.Ratio                     ( (%) )
import qualified Formatting                     ( fprint )
import qualified Formatting.Clock               ( timeSpecs )
import qualified System.Clock                   ( getTime
                                                , Clock(Monotonic)
                                                )

import qualified Coding
import qualified Evolutionary
import qualified Objectives
import qualified Utils


main :: IO ()
main = do
  arguments <- System.Environment.getArgs
  if Data.List.length arguments /= 1
    then
      putStrLn
        "Application Task1 takes only one argument - configPath, if it is not passed or if you pass more arguments default config \"config\\Task1\\config.txt\" is loaded."
    else putStrLn ("Application Task1 is loading config: " ++ head arguments)
  let configPath = if Data.List.length arguments == 1
        then head arguments
        else "config\\Task1\\config.txt"
  config <- getConfig configPath
  let objectiveFunction = Objectives.parseObjectiveFunction (function config)
  let functorValue                 = Objectives.functor objectiveFunction
  let objectiveFunctionStringValue = Objectives.string objectiveFunction
  let rangeXValue                  = Objectives.rangeX objectiveFunction
  let rangeYValue                  = Objectives.rangeY objectiveFunction
  let isoPointsValue               = Objectives.isoPoints objectiveFunction
  let groundLevelValue             = Objectives.groundLevel objectiveFunction
  generator <- Random.Xorshift.Int64.newXorshift64
  let encoding  = id -- Coding.grayCoding
  let decoding  = id -- Coding.grayDecoding
  let selection = Evolutionary.rouletteSelection generator
  let mutation =
        Evolutionary.flipBitMutation generator (mutationProbability config)
  let crossover =
        Evolutionary.onePointCrossover generator (crossoverProbability config)
  let outputDirectory = outputDir config
  exists <- System.Directory.doesDirectoryExist outputDirectory
  Control.Monad.when
    exists
    (System.Directory.removeDirectoryRecursive outputDirectory)
  let population = Utils.generatePopulation (populationSize config)
                                            2
                                            (features config)
                                            generator
                                            encoding
  let computedPoints = Utils.computePoints functorValue
                                           rangeXValue
                                           rangeYValue
                                           decoding
                                           population
  putStrLn "Best initial guess:"
  print (head (Utils.sortByObjectiveFunctionValue computedPoints))
  Utils.plot objectiveFunctionStringValue
             isoPointsValue
             groundLevelValue
             rangeXValue
             rangeYValue
             True
             0
             outputDirectory
             computedPoints
  Utils.plot objectiveFunctionStringValue
             isoPointsValue
             groundLevelValue
             rangeXValue
             rangeYValue
             False
             0
             outputDirectory
             computedPoints
  let iterateFunction = Evolutionary.nextGeneration decoding
                                                    selection
                                                    mutation
                                                    crossover
                                                    rangeXValue
                                                    rangeYValue
                                                    functorValue
  startComputing <- System.Clock.getTime System.Clock.Monotonic
  let results = take (iterations config)
                     (iterate iterateFunction (population, computedPoints))
  putStrLn "Result:"
  print (head (Utils.sortByObjectiveFunctionValue (snd (last results))))
  endComputing <- System.Clock.getTime System.Clock.Monotonic
  putStrLn "Processing time:"
  Formatting.fprint Formatting.Clock.timeSpecs startComputing endComputing
  putStrLn ""
  putStrLn "Saving output started!"
  startSaving <- System.Clock.getTime System.Clock.Monotonic
  let resultsWithIndexes = zip results [1, 2 .. (iterations config)]
  Control.Monad.mapM_
    (helper objectiveFunctionStringValue
            isoPointsValue
            groundLevelValue
            rangeXValue
            rangeYValue
            outputDirectory
    )
    resultsWithIndexes
  Utils.plot2D outputDirectory "best"
  Utils.plot2D outputDirectory "mean"
  endSaving <- System.Clock.getTime System.Clock.Monotonic
  putStrLn "Saving time:"
  Formatting.fprint Formatting.Clock.timeSpecs startSaving endSaving
  putStrLn "\nSaving output finished!"



helper
  :: String
  -> Integer
  -> Double
  -> (Double, Double)
  -> (Double, Double)
  -> String
  -> (([[[Bool]]], [[Double]]), Int)
  -> IO ()
helper objectiveFunctionStringValue isoPointsValue groundLevelValue rangeXValue rangeYValue outputDirectory x
  = do
    Utils.plot objectiveFunctionStringValue
               isoPointsValue
               groundLevelValue
               rangeXValue
               rangeYValue
               True
               (snd x)
               outputDirectory
               (snd (fst x))
    Utils.plot objectiveFunctionStringValue
               isoPointsValue
               groundLevelValue
               rangeXValue
               rangeYValue
               False
               (snd x)
               outputDirectory
               (snd (fst x))

data Config = Config
  { outputDir :: String
  ,function :: String
  , populationSize :: Int
  , features :: Int
  , iterations :: Int
  , mutationProbability :: Rational
  , crossoverProbability :: Rational
  } deriving (Eq, Show)

parseConfig :: Data.Ini.Config.IniParser Config
parseConfig = Data.Ini.Config.section "Task1" $ do
  outputDir      <- Data.Ini.Config.fieldOf "outputDir" Data.Ini.Config.string
  function       <- Data.Ini.Config.fieldOf "function" Data.Ini.Config.string
  populationSize <- Data.Ini.Config.fieldOf "populationSize"
                                            Data.Ini.Config.number
  features <- Data.Ini.Config.fieldOf "features" Data.Ini.Config.number
  iterations <- Data.Ini.Config.fieldOf "iterations" Data.Ini.Config.number
  mutationProbability <- Data.Ini.Config.fieldOf "mutationProbability"
                                                 Data.Ini.Config.number
  crossoverProbability <- Data.Ini.Config.fieldOf "crossoverProbability"
                                                  Data.Ini.Config.number
  return
    (Config outputDir
            function
            populationSize
            features
            iterations
            mutationProbability
            crossoverProbability
    )

getConfig path = do
  configFile <- readFile path
  let parsingResult =
        Data.Ini.Config.parseIniFile (Data.Text.pack configFile) parseConfig
  case parsingResult of
    Left message -> do
      putStrLn message
      putStrLn "Default config will be used!"
    Right config -> putStrLn "Config successfully loaded!"
  let defaultConfig = Config "output\\first"
                             "first"
                             1024
                             64
                             1000
                             (1 Data.Ratio.% 1000)
                             (6 Data.Ratio.% 10)
  let config = Data.Either.fromRight defaultConfig parsingResult
  print config
  return config
