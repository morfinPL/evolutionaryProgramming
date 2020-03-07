module Objectives where

firstFunction :: Double -> Double -> Double
firstFunction x y = x^2 + y^2 + 10 * sin(x) - 10 * cos(y) + 10 * sin(2 * x) * cos(2 * y)

firstFunctionString :: String
firstFunctionString = "x**2 + y**2 + 10 * sin(x) - 10 * cos(y) + 10 * sin(2 * x) * cos(2 * y)"
