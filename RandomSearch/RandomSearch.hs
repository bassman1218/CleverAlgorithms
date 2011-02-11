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

candidate :: Int -> Int -> Float -> Float -> [Float]
-- generate a list of length equal to the number of dimensions
candidate nDims seed min max = take nDims (randomList seed min max)

randomSearch :: (Float -> Float) -> ([Float] -> Float) -> Float -> Float -> Int -> Int -> Float
-- the top-level function takes the cost function, the optimization function to be applied
-- to the list of costs, the min and max range of values, the random seed and the number of iterations 
-- the seed is different for each iteration to ensure that different candidates are generated
randomSearch costFn optimizeFn min max seed iters =
	optimizeFn [cost costFn (candidate 2 (seed * n + 137) min max) | n <- [1..iters]]

main = do
          putStr "Value found is: "
	  let costFn = (\n -> n * n);
              optimizeFn = minimum;
              min = (-5.0);
              max = 5.0;
              seed = 42;
              iters = 1000;
              v = randomSearch costFn optimizeFn min max seed iters
          print v