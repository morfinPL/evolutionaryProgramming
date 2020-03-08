module Utils where

import qualified System.Process (rawSystem)
import qualified System.Random (StdGen, randomRs)
import qualified Data.List.Split (chunksOf)

plot :: String -> Integer -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
plot functionString samples planeLevel minX maxX minY maxY solutionX  solutionY solutionZ = do
    let args = ["set grid;",
                "set pm3d;",
                "set palette rgb 33,13,10;",
                "set object circle at first " ++
                show solutionX ++ "," ++
                show solutionY ++ "," ++
                show solutionZ ++ " " ++
                "radius char 0.5 fc rgb '#FF0000' " ++
                "fs solid border lc rgb '#FF0000' lw 5;",
                "set isosample " ++ show samples ++ ";",
                "set xyplane at " ++ show planeLevel ++ ";",
                "set xrange [" ++ show minX ++ ":" ++ show maxX ++ "];",
                "set yrange [" ++ show minY ++ ":" ++ show maxY ++ "];",
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
