module Objectives where


data ObjectiveFunction = ObjectiveFunction
    { functor :: Double -> Double -> Double
    , string :: String
    , isoPoints :: Integer
    , groundLevel :: Double
    , rangeX :: (Double, Double)
    , rangeY :: (Double, Double)
    }


parseObjectiveFunction :: String -> Objectives.ObjectiveFunction
parseObjectiveFunction s = case s of
                            "third" -> Objectives.thirdFunction
                            "second"   -> Objectives.secondFunction
                            _ -> Objectives.firstFunction


firstFunction :: ObjectiveFunction
firstFunction = ObjectiveFunction (\x y -> x^2 + y^2 + 10 * sin x - 10 * cos y  + 10 * sin(2 * x) * cos(2 * y))
                                   "x**2 + y**2 + 10 * sin(x) - 10 * cos(y) + 10 * sin(2 * x) * cos(2 * y)"
                                   50
                                   (-25)
                                   (-7.5, 7.5)
                                   (-7.5, 7.5)


secondFunction :: ObjectiveFunction
secondFunction = ObjectiveFunction (\x y -> -20 * exp(-0.2 * sqrt(0.5 * (x^2 + y^2))) - exp(0.5 * (cos(2 * pi * x) + cos(2 * pi * y))) + exp 1 + 20)
                                    "-20 * exp(-0.2 * sqrt(0.5 * (x**2 + y**2))) - exp(0.5 * (cos(2 * pi * x) + cos(2 * pi * y))) + exp(1) + 20"
                                    100
                                    0
                                    (-5, 5)
                                    (-5, 5)

thirdFunction :: ObjectiveFunction
thirdFunction = ObjectiveFunction (\x y -> 0.5 + (sin (x^2 - y^2)^2 -0.5)/((1+ 0.001 * (x^2 + y^2))^2))
                                  "0.5 + ((sin(x**2 - y**2))**2 -0.5)/((1+ 0.001 * (x**2 + y**2))**2)"
                                  100
                                  0
                                  (-100, 100)
                                  (-100, 100)