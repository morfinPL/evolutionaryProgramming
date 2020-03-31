module Task1 where

import qualified Control.Monad                  ( mapM_
                                                , when
                                                )
import qualified Data.List                      ( length )
import qualified Formatting                     ( fprint )
import qualified Formatting.Clock               ( timeSpecs )
import qualified System.Clock                   ( getTime
                                                , Clock(Monotonic)
                                                )
import qualified System.Environment             ( getArgs )
import qualified System.Directory               ( doesDirectoryExist
                                                , removeDirectoryRecursive
                                                )
import qualified System.Random.Mersenne.Pure64  ( newPureMT )

import qualified Coding
import qualified Configs
import qualified Crossovers
import qualified Evolutionary
import qualified Mutations
import qualified Objectives
import qualified Selections
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
  let dimensions = 2
  config <- Configs.loadTask1Config configPath
  let objectiveFunction =
        Objectives.parseObjectiveFunction (Configs.function config)
  let functorValue                 = Objectives.functor objectiveFunction
  let objectiveFunctionStringValue = Objectives.string objectiveFunction
  let rangeXValue                  = Objectives.rangeX objectiveFunction
  let rangeYValue                  = Objectives.rangeY objectiveFunction
  let isoPointsValue               = Objectives.isoPoints objectiveFunction
  let groundLevelValue             = Objectives.groundLevel objectiveFunction
  generator <- System.Random.Mersenne.Pure64.newPureMT
  let encoding = if Configs.sga config then id else Coding.grayCoding
  let decoding = if Configs.sga config then id else Coding.grayDecoding
  let selection = if Configs.sga config
        then Selections.roulette generator
        else Selections.champion generator
  let mutation = if Configs.sga config
        then Mutations.flipBit generator (Configs.mutationProbability config)
        else Mutations.reverseSequence generator
                                       (Configs.mutationProbability config)
  let crossover = if Configs.sga config
        then Crossovers.onePoint generator (Configs.crossoverProbability config)
        else Crossovers.randomPattern generator
                                      (Configs.crossoverProbability config)
  let outputDirectory = Configs.outputDir config
  let computePoints =
        Utils.computePoints functorValue rangeXValue rangeYValue decoding
  exists <- System.Directory.doesDirectoryExist outputDirectory
  Control.Monad.when
    exists
    (System.Directory.removeDirectoryRecursive outputDirectory)
  let population = Evolutionary.generatePopulation
        generator
        (Configs.populationSize config)
        dimensions
        (Configs.features config)
        encoding
  let computedPoints = Utils.computePoints functorValue
                                           rangeXValue
                                           rangeYValue
                                           decoding
                                           population
  putStrLn "Best initial guess:"
  print (head (Utils.sortByLastValue computedPoints))
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
                                                    computePoints
  startComputing <- System.Clock.getTime System.Clock.Monotonic
  let results = take (Configs.iterations config)
                     (iterate iterateFunction (population, computedPoints))
  putStrLn "Result:"
  print (head (Utils.sortByLastValue (snd (last results))))
  endComputing <- System.Clock.getTime System.Clock.Monotonic
  putStrLn "Processing time:"
  Formatting.fprint Formatting.Clock.timeSpecs startComputing endComputing
  putStrLn ""
  putStrLn "Saving output started!"
  startSaving <- System.Clock.getTime System.Clock.Monotonic
  let resultsWithIndexes = zip results [1, 2 .. (Configs.iterations config)]
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
