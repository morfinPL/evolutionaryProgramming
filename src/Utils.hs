module Utils where

import qualified System.Process                 ( rawSystem )
import qualified System.Random                  ( StdGen
                                                , randomRs
                                                )
import qualified Control.Monad                  ( when )
import qualified Data.List                      ( length
                                                , sortBy
                                                )
import qualified Data.List.Split                ( chunksOf )
import qualified Data.Bool                      ( bool )
import qualified System.Directory               ( createDirectoryIfMissing )
import qualified System.IO                      ( writeFile
                                                , appendFile
                                                )


pointToString :: [Double] -> String
pointToString point =
  show (head point)
    ++ " "
    ++ show (point !! 1)
    ++ " "
    ++ show (point !! 2)
    ++ "\n"

writePointsToFile :: String -> Int -> [[Double]] -> IO ()
writePointsToFile outputDir iteration points = do
  let sortedPoints = sortByObjectiveFunctionValue points
  let strings      = map pointToString sortedPoints
  System.Directory.createDirectoryIfMissing True (outputDir ++ "\\0_0")
  System.Directory.createDirectoryIfMissing True (outputDir ++ "\\75_45")
  System.Directory.createDirectoryIfMissing True (outputDir ++ "\\txt")
  System.IO.writeFile
    (outputDir ++ "\\txt\\population" ++ show iteration ++ ".txt")
    (concat strings)
  System.IO.writeFile (outputDir ++ "\\txt\\best" ++ show iteration ++ ".txt")
                      (head strings)
  System.IO.appendFile
    (outputDir ++ "\\txt\\progress.txt")
    (show iteration ++ "\t" ++ show (head sortedPoints !! 2) ++ "\n")

plot
  :: String
  -> Integer
  -> Double
  -> (Double, Double)
  -> (Double, Double)
  -> Bool
  -> Int
  -> String
  -> [[Double]]
  -> IO ()
plot functionString samples planeLevel rangeX rangeY up iteration outputDir points
  = do
    Control.Monad.when up (writePointsToFile outputDir iteration points)
    let
      args =
        [ "set title 'Iteration " ++ show iteration ++ "';"
        , "set grid;"
        , "set pm3d;"
        , "set view "
          ++ (if up then "0, 0" else "75, 45")
          ++ ", "
          ++ (if up then "1.1, 1.1;" else "1, 1;")
        , if up
          then "set colorbox vertical user origin .02,.2;"
          else "set colorbox horizontal user origin .1,.1  size .8,.04;"
        , "set palette rgb 33,13,10;"
        , "set isosample " ++ show samples ++ ";"
        , "set xyplane at " ++ show planeLevel ++ ";"
        , "set xrange ["
          ++ show (fst rangeX)
          ++ ":"
          ++ show (snd rangeX)
          ++ "];"
        , "set yrange ["
          ++ show (fst rangeY)
          ++ ":"
          ++ show (snd rangeY)
          ++ "];"
        , "splot "
          ++ functionString
          ++ " title 'Objective Function' with lines lc rgb '#000000', "
          ++ "'"
          ++ outputDir
          ++ "\\txt\\population"
          ++ show iteration
          ++ ".txt"
          ++ "' using 1:2:3 title 'Population' with points pt 7 lc rgb '#FF3333', "
        , "'"
          ++ outputDir
          ++ "\\txt\\best"
          ++ show iteration
          ++ ".txt"
          ++ "' using 1:2:3 title 'Best individual' with points pt 7 lc rgb '#00FF00';"
        ]
    let cmd =
          [ "-e"
          , "set terminal pngcairo size 1280,768;set output '"
            ++ outputDir
            ++ "\\"
            ++ (if up then "0_0" else "75_45")
            ++ "\\iteration_"
            ++ show iteration
            ++ "_"
            ++ ".png';"
            ++ concat args
          ]
    System.Process.rawSystem "gnuplot" cmd
    return ()

generatePopulation
  :: Int -> Int -> System.Random.StdGen -> ([Bool] -> [Bool]) -> [[Bool]]
generatePopulation populationSize numberOfFeatures generator coding = map
  coding
  (Data.List.Split.chunksOf
    numberOfFeatures
    (map
      booleaner
      (take (numberOfFeatures * populationSize)
            (System.Random.randomRs (0 :: Integer, 1 :: Integer) generator)
      )
    )
  )
  where booleaner x = x == 1


integerToDouble :: Int -> Integer -> Double
integerToDouble power x = (/) (fromIntegral x) (fromIntegral (2 ^ power - 1))

individualToPoint :: Int -> [Bool] -> [Double]
individualToPoint dimensions individual = do
  let power               = div (length individual) dimensions
  let coordinatesBooleans = Data.List.Split.chunksOf power individual
  let mappingFunction     = integerToDouble power
  map (mappingFunction . bin2dec) coordinatesBooleans
  where bin2dec = foldl (\a -> (+) (2 * a) . Data.Bool.bool 0 1) 0

convertPopulationToPoints :: Int -> ([Bool] -> [Bool]) -> [[Bool]] -> [[Double]]
convertPopulationToPoints dimensions decoding =
  map (individualToPoint dimensions . decoding)

scalePoints :: Num a => (a, a) -> Int -> [[a]] -> [[a]]
scalePoints range coordinate = map (scalePoint range coordinate)
 where
  scalePoint range coordinate point =
    take coordinate point
      ++ [(point !! coordinate) * (snd range - fst range) + fst range]
      ++ drop (coordinate + 1) point

compute :: (Double -> Double -> Double) -> [Double] -> [Double]
compute objectiveFunction point = do
  let x = head point
  let y = last point
  let z = objectiveFunction x y
  [x, y, z]

sortByObjectiveFunctionValue :: [[Double]] -> [[Double]]
sortByObjectiveFunctionValue =
  Data.List.sortBy (\xs ys -> compare (last xs) (last ys))

computePoints
  :: (Double -> Double -> Double)
  -> (Double, Double)
  -> (Double, Double)
  -> ([Bool] -> [Bool])
  -> [[Bool]]
  -> [[Double]]
computePoints objectiveFunction rangeX rangeY decoding population = do
  let populationPoints = Utils.convertPopulationToPoints 2 decoding population
  let populationScaledXPoints = Utils.scalePoints rangeX 0 populationPoints
  let populationScaledPoints =
        Utils.scalePoints rangeY 1 populationScaledXPoints
  map (Utils.compute objectiveFunction) populationScaledPoints

codeToString :: [Bool] -> String
codeToString = map boolToString where boolToString b = if b then '1' else '0'
