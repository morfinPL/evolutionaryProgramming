module Mutations where

import qualified Data.List.Split                ( chunksOf )
import qualified System.Random                  ( RandomGen )

import qualified Utils

flipBit
  :: System.Random.RandomGen g => g -> Rational -> [[[Bool]]] -> [[[Bool]]]
flipBit generator probability population = do
  let dimensions               = length (head population)
  let numberOfBitsPerDimension = length (head (head population))
  let flattenPopulation        = concat (concat population)
  let operations = take
        (length flattenPopulation)
        (Utils.weightedList generator
                            [(not, probability), (id, 1 - probability)]
        )
  let tuples = zip operations flattenPopulation
  Data.List.Split.chunksOf
    dimensions
    (Data.List.Split.chunksOf numberOfBitsPerDimension (map apply tuples))
  where apply a = fst a (snd a)
