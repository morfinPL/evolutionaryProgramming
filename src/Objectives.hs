module Objectives where

firstFunction :: Double -> Double -> Double
firstFunction x y = x^2 + y^2 + 10 * sin x - 10 * cos y  + 10 * sin(2 * x) * cos(2 * y)

firstFunctionString :: String
firstFunctionString = "x**2 + y**2 + 10 * sin(x) - 10 * cos(y) + 10 * sin(2 * x) * cos(2 * y)"

secondFunction :: Double -> Double -> Double
secondFunction x y = -20 * exp(-0.2 * sqrt(0.5 * (x^2 + y^2))) - exp(0.5 * (cos(2 * pi * x) + cos(2 * pi * y))) + exp 1 + 20

secondFunctionString :: String
secondFunctionString = "-20 * exp(-0.2 * sqrt(0.5 * (x**2 + y**2))) - exp(0.5 * (cos(2 * pi * x) + cos(2 * pi * y))) + exp(1) + 20"
