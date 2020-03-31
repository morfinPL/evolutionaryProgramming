module Task1 where

import qualified Control.Monad                  ( mapM_
                                                , when
                                                )
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
  if length arguments /= 1
    then
      putStrLn
        "\nApplication Task1 takes only one argument - configPath, if you pass different number of arguments default config \"config\\Task1\\config.txt\" is loaded.\n"
    else putStrLn
      ("\nApplication Task1 is loading config: " ++ head arguments ++ "\n")
  let configPath = if length arguments == 1
        then head arguments
        else "config\\Task1\\config.txt"

  let dimensions = 2
  config <- Configs.loadTask1Config configPath
  let objectiveFunction =
        Objectives.parseObjectiveFunction (Configs.function config)
  let functor                 = Objectives.functor objectiveFunction
  let objectiveFunctionString = Objectives.string objectiveFunction
  let rangeX                  = Objectives.rangeX objectiveFunction
  let rangeY                  = Objectives.rangeY objectiveFunction
  let isoPoints               = Objectives.isoPoints objectiveFunction
  let groundLevel             = Objectives.groundLevel objectiveFunction
  let sga                     = Configs.sga config
  let visualization           = Configs.visualization config
  let outputDirectory         = Configs.outputDir config

  generator <- System.Random.Mersenne.Pure64.newPureMT
  let encoding = if sga then id else Coding.grayCoding
  let decoding = if sga then id else Coding.grayDecoding
  let selection = if sga
        then Selections.roulette generator
        else Selections.champion generator
  let mutation = if sga
        then Mutations.flipBit generator (Configs.mutationProbability config)
        else Mutations.reverseSequence generator
                                       (Configs.mutationProbability config)
  let crossover = if sga
        then Crossovers.onePoint generator (Configs.crossoverProbability config)
        else Crossovers.randomPattern generator
                                      (Configs.crossoverProbability config)
  let computePoints = Utils.computePoints functor rangeX rangeY decoding

  exists <- System.Directory.doesDirectoryExist outputDirectory
  Control.Monad.when
    (exists && visualization)
    (System.Directory.removeDirectoryRecursive outputDirectory)

  let population = Evolutionary.generatePopulation
        generator
        (Configs.populationSize config)
        dimensions
        (Configs.features config)
        encoding
  let computedPoints =
        Utils.computePoints functor rangeX rangeY decoding population

  putStrLn "\nBest initial guess:"
  print
    (fst (Utils.findBestIndividualInIteration ((population, computedPoints), 0))
    )

  Control.Monad.when
    visualization
    (Utils.plot2DObjectiveFunctionVisualizationFromTwoPerspectives
      objectiveFunctionString
      isoPoints
      groundLevel
      rangeX
      rangeY
      outputDirectory
      ((population, computedPoints), 0)
    )

  let nextGeneration = Evolutionary.nextGeneration decoding
                                                   selection
                                                   mutation
                                                   crossover
                                                   computePoints
  putStrLn "\nProcessing started!\n"
  startComputing <- System.Clock.getTime System.Clock.Monotonic
  let results = zip
        (take (Configs.iterations config)
              (iterate nextGeneration (population, computedPoints))
        )
        [1, 2 .. (Configs.iterations config)]

  putStrLn "Best individual (best individual, iteration):"
  print (Utils.findBestIndividualInResults results)
  endComputing <- System.Clock.getTime System.Clock.Monotonic
  putStrLn "\nProcessing time:"
  Formatting.fprint Formatting.Clock.timeSpecs startComputing endComputing
  putStrLn ""

  Control.Monad.when visualization $ do
    putStrLn "\nSaving output started!\n"
    startSaving <- System.Clock.getTime System.Clock.Monotonic
    Control.Monad.mapM_
      (Utils.plot2DObjectiveFunctionVisualizationFromTwoPerspectives
        objectiveFunctionString
        isoPoints
        groundLevel
        rangeX
        rangeY
        outputDirectory
      )
      results
    Utils.plot2D outputDirectory "best"
    Utils.plot2D outputDirectory "mean"
    endSaving <- System.Clock.getTime System.Clock.Monotonic
    putStrLn "Saving time:"
    Formatting.fprint Formatting.Clock.timeSpecs startSaving endSaving
    putStrLn ""
