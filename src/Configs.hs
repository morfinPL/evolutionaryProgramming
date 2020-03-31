{-# LANGUAGE OverloadedStrings #-}

module Configs where

import qualified Data.Either                    ( fromRight )
import qualified Data.Ini.Config                ( IniParser
                                                , section
                                                , fieldOf
                                                , number
                                                , flag
                                                , string
                                                , parseIniFile
                                                )
import qualified Data.Ratio                     ( (%) )
import qualified Data.Text                      ( pack )


data Task1 = Task1
  { outputDir :: String
  ,function :: String
  , populationSize :: Int
  , features :: Int
  , iterations :: Int
  , mutationProbability :: Rational
  , crossoverProbability :: Rational
  , sga :: Bool
  } deriving (Eq, Show)

parseConfig :: Data.Ini.Config.IniParser Task1
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
  sga <- Data.Ini.Config.fieldOf "sga" Data.Ini.Config.flag
  return
    (Task1 outputDir
           function
           populationSize
           features
           iterations
           mutationProbability
           crossoverProbability
           sga
    )

loadTask1Config path = do
  configFile <- readFile path
  let parsingResult =
        Data.Ini.Config.parseIniFile (Data.Text.pack configFile) parseConfig
  case parsingResult of
    Left message -> do
      putStrLn message
      putStrLn "Default config will be used!"
    Right config -> putStrLn "Config successfully loaded!"
  let defaultConfig = Task1 "output\\first"
                            "first"
                            1024
                            32
                            100
                            (1 Data.Ratio.% 1000)
                            (6 Data.Ratio.% 10)
                            True
  let config = Data.Either.fromRight defaultConfig parsingResult
  print config
  return config
