module Evolutionary where

import qualified Control.Monad.Random           ( evalRand
                                                , fromList
                                                )
import qualified Data.List                      ( length
                                                , elemIndices
                                                , find
                                                )
import qualified Data.List.Split                ( chunksOf )
import qualified System.Random                  ( RandomGen
                                                , randoms
                                                , randomRs
                                                )

import qualified Utils                          ( computePoints )


computeProbabilities :: Double -> [Double] -> [Double]
computeProbabilities sum = map (divide sum) where divide sum elem = elem / sum


roulette :: [[Double]] -> [Double]
roulette points = do
  let values = map take3rdElement points where take3rdElement list = list !! 2
  let max          = maximum values
  let increasedMax = max + abs (0.1 * max)
  let differences = computeDifferences increasedMax values
        where computeDifferences max = map (subtract max)
  let sumOfDifferences = sum differences
  computeSumOfProbabilities (computeProbabilities sumOfDifferences differences)
  where computeSumOfProbabilities list = scanl1 (+) (init list) ++ [1.0]


choicesToIndexes :: [Double] -> [Double] -> [Int]
choicesToIndexes choices roulette = map (choiceToIndex roulette) choices
 where
  choiceToIndex roulette a = head
    (Data.List.elemIndices (Data.List.find (>= a) roulette) (map Just roulette))


generateNewPopulationByRoulette
  :: System.Random.RandomGen g => g -> [[[Bool]]] -> [Double] -> [[[Bool]]]
generateNewPopulationByRoulette generator oldPopulation roulette = do
  let choices = take (length oldPopulation)
                     (System.Random.randoms generator :: [Double])
  map (indexToIndividual oldPopulation) (choicesToIndexes choices roulette)
  where indexToIndividual oldPopulation index = oldPopulation !! index


weightedList :: System.Random.RandomGen g => g -> [(a, Rational)] -> [a]
weightedList generator weights = Control.Monad.Random.evalRand m generator
  where m = sequence . repeat . Control.Monad.Random.fromList $ weights


mutate :: System.Random.RandomGen g => g -> Rational -> [[[Bool]]] -> [[[Bool]]]
mutate generator probability population = do
  let dimensions               = length (head population)
  let numberOfBitsPerDimension = length (head (head population))
  let flattenPopulation        = concat (concat population)
  let operations = take
        (length flattenPopulation)
        (weightedList generator [(not, probability), (id, 1 - probability)])
  let tuples = zip operations flattenPopulation
  Data.List.Split.chunksOf
    dimensions
    (Data.List.Split.chunksOf numberOfBitsPerDimension (map apply tuples))
  where apply a = fst a (snd a)


cross :: (([Bool], [Bool]), Int) -> ([Bool], [Bool])
cross tuple = do
  let coordinate = snd tuple
  let parents    = fst tuple
  let parentA    = fst parents
  let parentB    = snd parents
  let childA = take coordinate parentA ++ drop coordinate parentB
  let childB = take coordinate parentB ++ drop coordinate parentA
  (childA, childB)

crossover :: ((([Bool], [Bool]), Int), Bool) -> ([Bool], [Bool])
crossover bigTuple =
  if snd bigTuple then cross (fst bigTuple) else fst (fst bigTuple)

coordinatesToIndividuals :: [([Bool], [Bool])] -> [[[Bool]]]
coordinatesToIndividuals pairCoordinates = do
  let childA = map fst pairCoordinates
  let childB = map snd pairCoordinates
  [childA, childB]

crossoverPopulation
  :: System.Random.RandomGen g => g -> Rational -> [[[Bool]]] -> [[[Bool]]]
crossoverPopulation generator probability population = do
  let dimensions         = length (head population)
  let pairs              = Data.List.Split.chunksOf 2 population
  let pairsOfCoordinates = map (uncurry zip . (\x -> (head x, last x))) pairs
  let crossoverIndicators = concatMap
        (replicate dimensions)
        (take
          (length pairs)
          (weightedList generator
                        [(True, probability), (False, 1 - probability)]
          )
        )
  let pointsOfCrossover = take
        (length pairs * dimensions)
        (System.Random.randomRs
          (1 :: Int, length (head (head population)) - 1)
          generator
        )
  let coordinatesWithPointsOfCrossoverAndIndicators = zip
        (zip (concat pairsOfCoordinates) pointsOfCrossover)
        crossoverIndicators
  let crossedCoordinates =
        map crossover coordinatesWithPointsOfCrossoverAndIndicators
  concatMap coordinatesToIndividuals
            (Data.List.Split.chunksOf dimensions crossedCoordinates)


nextGeneration
  :: System.Random.RandomGen g
  => g
  -> Rational
  -> Rational
  -> ([Bool] -> [Bool])
  -> (Double, Double)
  -> (Double, Double)
  -> (Double -> Double -> Double)
  -> ([[[Bool]]], [[Double]])
  -> ([[[Bool]]], [[Double]])
nextGeneration generator mutationProbability crossoverProbability decoding rangeX rangeY objectiveFunction population
  = do
    let oldPopulation = fst population
    let oldPoints     = snd population
    let probabilities = roulette oldPoints
    let parents =
          generateNewPopulationByRoulette generator oldPopulation probabilities
    let newPopulation = crossoverPopulation
          generator
          crossoverProbability
          (mutate generator mutationProbability parents)
    let newPoints = Utils.computePoints objectiveFunction
                                        rangeX
                                        rangeY
                                        decoding
                                        newPopulation
    (newPopulation, newPoints)

xor :: Bool -> Bool -> Bool
xor p q = (p && not q) || (not p && q)

xorArray :: [(Bool, Bool)] -> [Bool]
xorArray = map (uncurry xor)

binaryToGrayCode :: [Bool] -> [Bool]
binaryToGrayCode []           = []
binaryToGrayCode [b         ] = [b]
binaryToGrayCode (b : c : bs) = b `xor` c : binaryToGrayCode (c : bs)

grayCodeToBinary :: [Bool] -> [Bool]
grayCodeToBinary = foldr go []
 where
  go c []         = [c]
  go c bs@(b : _) = b `xor` c : bs
