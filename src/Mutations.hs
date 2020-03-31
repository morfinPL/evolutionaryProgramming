module Mutations where

import qualified Data.List                      ( sort )
import qualified Data.List.Split                ( chunksOf )
import qualified System.Random                  ( RandomGen
                                                , randomRs
                                                )

import qualified Utils

flipBitIndividual :: System.Random.RandomGen g => g -> [[Bool]] -> [[Bool]]
flipBitIndividual generator individual = do
  let bits                   = length (head individual)
  let concatenatedIndividual = concat individual
  let index = head
        (System.Random.randomRs
          (0 :: Int, length concatenatedIndividual - 1 :: Int)
          generator
        )
  Data.List.Split.chunksOf
    bits
    (  take index concatenatedIndividual
    ++ [concatenatedIndividual !! index]
    ++ drop (index + 1) concatenatedIndividual
    )

flipBit
  :: System.Random.RandomGen g => g -> Rational -> [[[Bool]]] -> [[[Bool]]]
flipBit generator probability population = do
  let mutationIndicator = take
        (length population)
        (Utils.weightedList generator
                            [(True, probability), (False, 1 - probability)]
        )
  zipWith (curry flipBitIf) population mutationIndicator where
  flipBitIf tuple =
    if snd tuple then flipBitIndividual generator (fst tuple) else fst tuple


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
