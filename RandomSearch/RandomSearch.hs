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
cost costFn lst = foldr1 (+) [costFn x | x <- lst]



randomSearch :: (Float -> Float) -> ([Float] -> Float) -> Int -> Float -> Float -> Int -> Int -> Float
-- the top-level function takes the cost function, the optimization function to be applied
-- to the list of costs, the min and max range of values, the random seed and the number of iterations 
-- the seed is different for each iteration to ensure that different candidates are generated
randomSearch costFn optimizeFn nDims min max seed iters =
	let randNums = randomList seed min max;
	    candidate :: Int -> [Float]
	    -- generate a list of length equal to the number of dimensions
            candidate iter = take nDims (drop ((iter - 1) * nDims) randNums)
	in
	optimizeFn [cost costFn (candidate n) | n <- [1..iters]]

randomSearch2 :: (Float -> Float) -> ([Float] -> Float) -> Int -> Float -> Float -> Int -> Int -> Float
-- In this version the list of random numbers is split into groups of length equal to number of dimensions
-- and the cost function is applied to each group
-- The seed is only used once and the overall speed is much better
randomSearch2 costFn optimizeFn nDims min max seed iters =
	let randNums = randomList seed min max;
	    costs :: Int -> [Float] -> Int -> [Float]
	    costs nDims lst 0 = []
            costs nDims lst iters = let (candidate, rest) = splitAt nDims lst
			            in
                                    cost costFn candidate : costs nDims rest (iters - 1) 
        in
	optimizeFn $ costs nDims randNums iters

main seed iters = do
          putStr "Value found is: "
	  let costFn = (\n -> n * n);
              optimizeFn = minimum;
              min = (-5.0);
              max = 5.0;
	      nDims = 2;
              v = randomSearch2 costFn optimizeFn nDims min max seed iters
          print v