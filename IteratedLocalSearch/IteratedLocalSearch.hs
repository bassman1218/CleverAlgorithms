-----------------------------------------------------------------
--                                                             --
--	IteratedLocalSearch.hs                                 --
--                                                             --
--	Iterated Local Search explores a sequence of solutions --
--      created as perturbations of the current best solution, --
--      the result of which is refined using an embedded       --
--      heuristic..                                            --                                                 --                                        --
--                                                             --
-----------------------------------------------------------------

module IteratedLocalSearch where

-- the random library: randoms is an infinite list of random numbers between 0 and 1
import System.Random

randomList :: Int -> [Float]
-- make an RNG using the seed and return the infinite list of randoms
randomList seed = randoms (mkStdGen seed)

-- an XY coordinate
type Point = (Float, Float)

square :: Float -> Float
square n = n * n

euclid2d :: Point -> Point -> Float
euclid2d (x1, y1) (x2, y2) = sqrt(square(x1 - x2) + square(y1 - y2))

permutationPairs :: [Int] -> [(Int, Int)]
permutationPairs list = zip list ((drop 1 list) ++ (take 1 list))

cost :: [Int] -> [Point] -> Float
cost permutation cities = foldr (+) 0.0 [euclid2d (cities !! n1) (cities !! n2) | (n1, n2) <- permutationPairs permutation]

randomPermutation :: [Int] -> [Float] -> [Int]
randomPermutation [last] _ = [last]
randomPermutation (first:rest) randNums =
	let [r] = take 1 randNums
	    len = (length rest) - 1
	    pos = round ((fromIntegral len) * r)
	in  (rest !! pos) : (randomPermutation ((take pos rest) ++ [first] ++ (drop (pos + 1) rest))  (drop 1 randNums))

