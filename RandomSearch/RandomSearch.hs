-----------------------------------------------------------------
--                                                             --
--	RandomSearch.hs                                        --
--                                                             --
--	This implements a simple sub-optimal random search in  --
--	a bounded n-dimensional space of floating-point values --
--                                                             --
-----------------------------------------------------------------

module RandomSearch where

-- the random library: randoms is an infinite list of random numbers between 0 and 1
import System.Random

randomList :: Int -> Float -> Float -> [Float]
-- normalize the infinite list of random numbers to lie between min and max
randomList seed min max = [x * (max - min) + min | x <- (randoms (mkStdGen seed))]

cost :: (Float -> Float ) -> [Float] -> Float
-- sum the values of the cost function applied to each element of the list
cost costFn lst = foldr (\n acc -> acc + (costFn n)) 0 lst

randomSearch :: (Float -> Float) -> ((Float, [Float]) -> (Float, [Float]) -> (Float, [Float])) -> Int -> Float -> Float -> Int -> Int -> (Float, [Float])
-- In this version the list of random numbers is split into groups of length equal to number of dimensions
-- and the cost function is applied to each group
-- The seed is only used once and the overall speed is much better
randomSearch costFn optimizeFn nDims min max seed iters =
	let randNums = randomList seed min max;
	    costs :: Int -> [Float] -> Int -> [(Float,[Float])]
	    costs nDims lst 0 = []
            costs nDims lst iters = let (candidate, rest) = splitAt nDims lst
			            in
                                    (cost costFn candidate, candidate) : costs nDims rest (iters - 1) 
        in
	foldr1 minCost (costs nDims randNums iters)

minCost :: (Float, [Float]) -> (Float, [Float]) -> (Float, [Float])
minCost p1@(c1, _) p2@(c2, _) = if c1 < c2 then p1 else p2

main seed iters = do
          putStr "Value found is: "
	  let costFn = (\n -> n * n);
              optimizeFn = minCost;
              min = (-5.0);
              max = 5.0;
	      nDims = 2;
              v = randomSearch costFn optimizeFn nDims min max seed iters
          print v