module Utils where

import qualified Objectives

import qualified System.Process (rawSystem)
import qualified System.Random (StdGen, randomRs)
import qualified Data.List (length, sortBy)
import qualified Data.List.Split (chunksOf)
import qualified Data.Bool (bool)
import qualified System.IO (writeFile)

pointToString :: [Double] -> String
pointToString point = head [show (head point) ++ " " ++
                            show (point !! 1) ++ " " ++
                            show (point !! 2) ++ "\n"]

writePointsToFile :: String -> [[Double]] -> IO ()
writePointsToFile filename points = do
                         let strings = map pointToString points
                         System.IO.writeFile filename (concat strings)

plot :: String -> String -> Integer -> Double -> (Double, Double) -> (Double, Double) -> [[Double]] -> IO ()
plot title functionString samples planeLevel rangeX rangeY points = do
    let filename = "output\\population.txt"
    writePointsToFile filename points
    let args = ["set title '" ++ title ++ "';",
                "set grid;",
                "set pm3d;",
                "set palette rgb 33,13,10;",
                "set isosample " ++ show samples ++ ";",
                "set xyplane at " ++ show planeLevel ++ ";",
                "set xrange [" ++ show (fst rangeX) ++ ":" ++ show (snd rangeX) ++ "];",
                "set yrange [" ++ show (fst rangeY) ++ ":" ++ show (snd rangeY) ++ "];",
                "splot " ++ functionString ++ " title 'Objective Function' with lines lc rgb '#000000';" ++
                "replot '" ++ filename ++ "' using 1:2:3 title 'Population' with points pt 7 lc rgb '#FF3333';"]
    System.Process.rawSystem "gnuplot" ["-persist", "-e", concat args]
    return ()

generatePopulation :: Int -> Int -> System.Random.StdGen -> [[Bool]]
generatePopulation populationSize numberOfFeatures generator = Data.List.Split.chunksOf numberOfFeatures
                                                               (map booleaner
                                                               (take (numberOfFeatures * populationSize)
                                                               (System.Random.randomRs (0 :: Integer, 1 :: Integer)
                                                               generator)))
                                                               where booleaner x = x == 1


integerToDouble :: Int -> Integer -> Double
integerToDouble power x = (/) (fromIntegral x) (fromIntegral (2^power -1))

individualToPoint :: Int -> [Bool] -> [Double]
individualToPoint dimensions individual = do
                                    let power = div (length individual) dimensions
                                    let coordinatesBooleans = Data.List.Split.chunksOf power individual
                                    let mappingFunction = integerToDouble power
                                    map (mappingFunction . bin2dec) coordinatesBooleans
                                        where bin2dec = foldl (\a -> (+) (2*a) . Data.Bool.bool 0 1) 0

convertPopulationToPoints :: Int -> [[Bool]] -> [[Double]]
convertPopulationToPoints dimensions = map (individualToPoint dimensions)

scalePoints :: Num a => (a, a) -> Int -> [[a]] -> [[a]]
scalePoints range coordinate = map (scalePoint range coordinate)
    where scalePoint range coordinate point = take coordinate point ++ [(point !! coordinate) * (snd range -fst range) + fst range] ++ drop (coordinate+1) point

compute :: (Double -> Double -> Double) -> [Double] -> [Double]
compute objectiveFunction point = do
                                  let x = head point
                                  let y = point !! 1
                                  let z = objectiveFunction x y
                                  [x, y, z]

lsort :: [[Double]] -> [[Double]]
lsort = Data.List.sortBy (\xs ys -> compare (xs !! 2) (ys !! 2))

computePoints :: (Double -> Double -> Double) -> (Double, Double) -> (Double, Double) -> [[Bool]] -> [[Double]]
computePoints objectiveFunction rangeX rangeY population = do
                                                           let populationPoints = Utils.convertPopulationToPoints 2 population
                                                           let populationScaledXPoints = Utils.scalePoints rangeX 0 populationPoints
                                                           let populationScaledPoints = Utils.scalePoints rangeY 1 populationScaledXPoints
                                                           map (Utils.compute Objectives.firstFunction) populationScaledPoints
