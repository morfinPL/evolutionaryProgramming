module Utils where
import System.Process (rawSystem)

plot :: String -> Integer -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> IO ()
plot functionString samples planeLevel minX maxX minY maxY solutionX  solutionY solutionZ  = do
    let gnuplotExecutable = "gnuplot"
    let interactiveOption = "-persist"
    let executeOption = "-e"
    let args = ["set grid;",
                "set pm3d;",
                "set palette rgb 33,13,10;",
                "set object circle at first " ++ show solutionX ++"," ++ show solutionY ++ "," ++ show solutionZ ++ " radius char 0.5 fc rgb '#FF0000' fs solid border lc rgb '#FF0000' lw 5;",
                "set isosample " ++ show samples ++ ";",
                "set xyplane at " ++ show planeLevel ++ ";",
                "set xrange [" ++ show minX ++ ":" ++ show maxX ++ "];",
                "set yrange [" ++ show minY ++ ":" ++ show maxY ++ "];",
                "splot ",
                functionString]
    rawSystem gnuplotExecutable [interactiveOption, executeOption, concat args]
    return ()
