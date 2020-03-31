module Utils where

import qualified Control.Monad.Random           ( evalRand
                                                , fromList
                                                )
import qualified Control.Monad                  ( when
                                                , unless
                                                )
import qualified Data.Bool                      ( bool )
import qualified Data.List                      ( length
                                                , sortBy
                                                )
import qualified System.Directory               ( createDirectoryIfMissing
                                                , removeFile
                                                )
import qualified System.IO                      ( writeFile
                                                , appendFile
                                                )
import qualified System.Process                 ( rawSystem )
import qualified System.Random                  ( RandomGen )


weightedList :: System.Random.RandomGen g => g -> [(a, Rational)] -> [a]
weightedList generator weights = Control.Monad.Random.evalRand m generator
  where m = sequence . repeat . Control.Monad.Random.fromList $ weights


scalePoints :: Num a => (a, a) -> Int -> [[a]] -> [[a]]
scalePoints range coordinate = map (scalePoint range coordinate)
 where
  scalePoint range coordinate point =
    take coordinate point
      ++ [(point !! coordinate) * (snd range - fst range) + fst range]
      ++ drop (coordinate + 1) point


computePoints
  :: (Double -> Double -> Double)
  -> (Double, Double)
  -> (Double, Double)
  -> ([Bool] -> [Bool])
  -> [[[Bool]]]
  -> [[Double]]
computePoints objectiveFunction rangeX rangeY decoding population = do
  let
    populationPoints = convertPopulationToPoints decoding population     where
      convertPopulationToPoints decoding = map (individualToPoint decoding)       where
        individualToPoint decoding individual = do
          let dimensions = length individual
          let bits       = length (head individual)
          let mappingFunction = integerToDouble bits               where
                integerToDouble power x =
                  fromIntegral x / fromIntegral (2 ^ power - 1)
          map (mappingFunction . bin2dec . decoding) individual
          where bin2dec = foldl (\a -> (+) (2 * a) . Data.Bool.bool 0 1) 0
  let populationScaledXPoints = Utils.scalePoints rangeX 0 populationPoints
  let populationScaledPoints =
        Utils.scalePoints rangeY 1 populationScaledXPoints
  map (compute objectiveFunction) populationScaledPoints where
  compute objectiveFunction point = do
    let x = head point
    let y = last point
    let z = objectiveFunction x y
    [x, y, z]


sortByLastValue :: (Ord a) => [[a]] -> [[a]]
sortByLastValue = Data.List.sortBy (\xs ys -> compare (last xs) (last ys))

writePointsToFile :: String -> Int -> [[Double]] -> IO ()
writePointsToFile outputDirectory iteration points = do
  let sortedPoints = sortByLastValue points
  let bestValue = last (head sortedPoints)
  let mean = sum (map last sortedPoints) / fromIntegral (length sortedPoints)
  let strings = map pointToString sortedPoints       where
        pointToString point =
          show (head point)
            ++ " "
            ++ show (point !! 1)
            ++ " "
            ++ show (point !! 2)
            ++ "\n"
  System.Directory.createDirectoryIfMissing True (outputDirectory ++ "\\0_0")
  System.Directory.createDirectoryIfMissing True (outputDirectory ++ "\\75_45")
  System.Directory.createDirectoryIfMissing True (outputDirectory ++ "\\txt")
  System.IO.writeFile
    (outputDirectory ++ "\\txt\\population" ++ show iteration ++ ".txt")
    (concat strings)
  System.IO.writeFile
    (outputDirectory ++ "\\txt\\best" ++ show iteration ++ ".txt")
    (head strings)
  System.IO.appendFile (outputDirectory ++ "\\txt\\best.txt")
                       (show iteration ++ "\t" ++ show bestValue ++ "\n")
  System.IO.appendFile (outputDirectory ++ "\\txt\\mean.txt")
                       (show iteration ++ "\t" ++ show mean ++ "\n")

deleteTempFiles :: String -> Int -> IO ()
deleteTempFiles outputDirectory iteration = do
  System.Directory.removeFile
    (outputDirectory ++ "\\txt\\population" ++ show iteration ++ ".txt")
  System.Directory.removeFile
    (outputDirectory ++ "\\txt\\best" ++ show iteration ++ ".txt")


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
plot functionString samples planeLevel rangeX rangeY up iteration outputDirectory points
  = do
    Control.Monad.when up (writePointsToFile outputDirectory iteration points)
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
          ++ outputDirectory
          ++ "\\txt\\population"
          ++ show iteration
          ++ ".txt"
          ++ "' using 1:2:3 title 'Population' with points pt 7 lc rgb '#FF3333', "
        , "'"
          ++ outputDirectory
          ++ "\\txt\\best"
          ++ show iteration
          ++ ".txt"
          ++ "' using 1:2:3 title 'Best individual' with points pt 7 lc rgb '#00FF00';"
        ]
    let cmd =
          [ "-e"
          , "set terminal pngcairo size 1280,768;set output '"
            ++ outputDirectory
            ++ "\\"
            ++ (if up then "0_0" else "75_45")
            ++ "\\iteration_"
            ++ show iteration
            ++ "_"
            ++ ".png';"
            ++ concat args
          ]
    System.Process.rawSystem "gnuplot" cmd
    Control.Monad.unless up (deleteTempFiles outputDirectory iteration)
    return ()


plot2D :: String -> String -> IO ()
plot2D outputDirectory title = do
  let
    args =
      (if title == "mean"
          then
            "set title 'Mean objective function value for whole population';set xlabel 'Iteration';set ylabel 'Objective function value';"
          else
            "set title 'Objective function value best individual';set xlabel 'Iteration';set ylabel 'Objective function value';"
        )
        ++ "plot '"
        ++ outputDirectory
        ++ "\\txt\\"
        ++ title
        ++ ".txt' using 1:2 title '"
        ++ (if title == "mean"
             then "Mean objective function value"
             else "Best objective function value"
           )
        ++ "' with linespoints lc rgb '#00AD60' lt 1 lw 2 pt 7 ps 1.5;"
  let cmd =
        [ "-e"
        , "set terminal pngcairo size 1280,768;set output '"
          ++ outputDirectory
          ++ "\\"
          ++ title
          ++ ".png';"
          ++ args
        ]
  System.Process.rawSystem "gnuplot" cmd
  return ()
