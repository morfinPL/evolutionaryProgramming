{-# LANGUAGE OverloadedStrings #-}

module Task1 where

import qualified System.Random
import qualified Data.Text                      ( pack )
import qualified Data.Ini.Config                ( IniParser
                                                , section
                                                , fieldOf
                                                , number
                                                , string
                                                , parseIniFile
                                                )
import qualified Data.Either                    ( fromRight )
import qualified Data.Ratio                     ( (%) )
import qualified Formatting                     ( fprint )
import qualified Formatting.Clock               ( timeSpecs )
import qualified System.Clock                   ( getTime
                                                , Clock(Monotonic)
                                                )

import qualified Evolutionary
import qualified Objectives
import qualified Utils


main :: IO ()
main = do
  config <- getConfig
  let objectiveFunction = Objectives.parseObjectiveFunction (function config)
  let functorValue                 = Objectives.functor objectiveFunction
  let objectiveFunctionStringValue = Objectives.string objectiveFunction
  let rangeXValue                  = Objectives.rangeX objectiveFunction
  let rangeYValue                  = Objectives.rangeY objectiveFunction
  let isoPointsValue               = Objectives.isoPoints objectiveFunction
  let groundLevelValue             = Objectives.groundLevel objectiveFunction
  let encoding                     = Evolutionary.binaryToGrayCode
  let decoding                     = Evolutionary.grayCodeToBinary
  generator <- System.Random.getStdGen

  let population = Utils.generatePopulation (populationSize config)
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
  Utils.plot "Initial population"
             objectiveFunctionStringValue
             isoPointsValue
             groundLevelValue
             rangeXValue
             rangeYValue
             computedPoints
  start <- System.Clock.getTime System.Clock.Monotonic
  let iterateFunction = Evolutionary.nextGeneration
        generator
        (mutationProbability config)
        (crossoverProbability config)
        decoding
        rangeXValue
        rangeYValue
        functorValue
  let newPopulation =
        iterate iterateFunction (population, computedPoints)
          !! (iterations config - 1)

  putStrLn "Solution:"
  print (head (Utils.sortByObjectiveFunctionValue (snd newPopulation)))
  end <- System.Clock.getTime System.Clock.Monotonic
  putStrLn "Processing time:"
  Formatting.fprint Formatting.Clock.timeSpecs start end
  Utils.plot "Final population"
             objectiveFunctionStringValue
             isoPointsValue
             groundLevelValue
             rangeXValue
             rangeYValue
             (snd newPopulation)


data Config = Config
  { function :: String
  , populationSize :: Int
  , features :: Int
  , iterations :: Int
  , mutationProbability :: Rational
  , crossoverProbability :: Rational
  } deriving (Eq, Show)

parseConfig :: Data.Ini.Config.IniParser Config
parseConfig = Data.Ini.Config.section "Task1" $ do
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
    (Config function
            populationSize
            features
            iterations
            mutationProbability
            crossoverProbability
    )

getConfig = do
  configFile <- readFile "config\\Task1\\config.txt"
  let parsingResult =
        Data.Ini.Config.parseIniFile (Data.Text.pack configFile) parseConfig
  case parsingResult of
    Left message -> do
      putStrLn message
      putStrLn "Default config will be used!"
    Right config -> putStrLn "Config successfully loaded!"
  let defaultConfig =
        Config "first" 1024 64 1000 (1 Data.Ratio.% 1000) (6 Data.Ratio.% 10)
  let config = Data.Either.fromRight defaultConfig parsingResult
  print config
  return config
