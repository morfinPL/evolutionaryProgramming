module Mutations where

import qualified Data.List                      ( sort )
import qualified Data.List.Split                ( chunksOf )
import qualified System.Random                  ( RandomGen
                                                , randomRs
                                                )

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


reverseIndividual :: System.Random.RandomGen g => g -> [[Bool]] -> [[Bool]]
reverseIndividual generator individual = do
  let bits                   = length (head individual)
  let concatenatedIndividual = concat individual
  let indexes = Data.List.sort
        (take
          2
          (System.Random.randomRs
            (0 :: Int, length concatenatedIndividual - 1 :: Int)
            generator
          )
        )
  Data.List.Split.chunksOf
    bits
    (  take (head indexes) concatenatedIndividual
    ++ (reverse . take (last indexes - head indexes) . drop (head indexes))
         concatenatedIndividual
    ++ drop (last indexes) concatenatedIndividual
    )

reverseSequence
  :: System.Random.RandomGen g => g -> Rational -> [[[Bool]]] -> [[[Bool]]]
reverseSequence generator probability population = do
  let mutationIndicator = take
        (length population)
        (Utils.weightedList generator
                            [(True, probability), (False, 1 - probability)]
        )
  zipWith (curry reverseIf) population mutationIndicator where
  reverseIf tuple =
    if snd tuple then reverseIndividual generator (fst tuple) else fst tuple
