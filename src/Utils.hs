module Utils where

import qualified System.Process (rawSystem)
import qualified System.Random (StdGen, randomRs)
import qualified Data.List (length)
import qualified Data.List.Split (chunksOf)
import qualified Data.Bool(bool)

pointToCircle :: [Double] -> String
pointToCircle point = head ["set object circle at first " ++
                            show (head point) ++ "," ++
                            show (point !! 1) ++ "," ++
                            show (point !! 2) ++ " " ++
                            "radius char 0.5 fc rgb '#FF0000' " ++
                            "fs solid border lc rgb '#FF0000' lw 5;"]


pointsToCircles :: [[Double]] -> String
pointsToCircles points = do
                         let strings = map pointToCircle points
                         concat strings

plot :: String -> Integer -> Double -> (Double, Double) -> (Double, Double) -> [[Double]] -> IO ()
plot functionString samples planeLevel rangeX rangeY points = do
    let args = ["set grid;",
                "set pm3d;",
                "set palette rgb 33,13,10;",
                pointsToCircles points,
                "set isosample " ++ show samples ++ ";",
                "set xyplane at " ++ show planeLevel ++ ";",
                "set xrange [" ++ show (fst rangeX) ++ ":" ++ show (snd rangeX) ++ "];",
                "set yrange [" ++ show (fst rangeY) ++ ":" ++ show (snd rangeY) ++ "];",
                "splot " ++ functionString]
    System.Process.rawSystem "gnuplot" ["-persist", "-e", concat args]
    return ()

generatePopulation :: Int -> Int -> System.Random.StdGen -> [[Bool]]
generatePopulation populationSize numberOfFeatures generator = Data.List.Split.chunksOf numberOfFeatures
                                                               (map booleaner
                                                               (take (numberOfFeatures * populationSize)
                                                               (System.Random.randomRs (0 :: Integer, 1 :: Integer)
                                                               generator)))
                                                               where booleaner x = x == 1


bin2dec :: (Foldable f, Integral i) => f Bool -> i
bin2dec = foldl (\a -> (+) (2*a) . Data.Bool.bool 0 1) 0

integerToDouble :: Int -> Integer -> Double
integerToDouble power x = (/) (fromIntegral x) (fromIntegral (2^power -1))

individualToPoint :: Int -> [Bool] -> [Double]
individualToPoint dims individual = do
                                    let power = div (length individual) dims
                                    let coordinatesBooleans = Data.List.Split.chunksOf power individual
                                    let mappingFunction = integerToDouble power
                                    map (mappingFunction . bin2dec) coordinatesBooleans

scale :: (Double, Double) -> Int -> [Double] -> [Double]
scale range coordinate list = do
                              let notScaledLeft = take coordinate list
                              let notScaledRight = drop (coordinate+1) list
                              let scaledElem = (list !! coordinate) * (snd range -fst range) + fst range
                              notScaledLeft ++ [scaledElem] ++ notScaledRight

compute :: (Double -> Double -> Double) -> [Double] -> [Double]
compute objectiveFunction point = do
                                  let x = head point
                                  let y = point !! 1
                                  let z = objectiveFunction x y
                                  [x, y, z]
