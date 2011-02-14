-----------------------------------------------------------------
--                                                             --
--	AdaptiveRandomSearch.hs                                --
--                                                             --
--	This implements a sub-optimal random search in         --
--	a bounded n-dimensional space of floating-point values --
--	where the step size adapts to the shapte of the        --
--	solution space.                                        --
--                                                             --
-----------------------------------------------------------------

module AdaptiveRandomSearch where

-- the random library: randoms is an infinite list of random numbers between 0 and 1
import System.Random

randomList :: Int -> [Float]
-- normalize the infinite list of random numbers to lie between min and max
randomList seed = randoms (mkStdGen seed)

cost :: (Float -> Float ) -> [Float] -> Float
-- sum the values of the cost function applied to each element of the list
cost costFn lst = foldr (\n acc -> acc + (costFn n)) 0 lst

type Trace = [(Int, Float, Float, [Float])]

type Params = (Float, Float, Int, Int, Float, Float, Int, (Float -> Float))

aRandomSearch :: Float -> [Float] -> Int -> Params -> [Float] -> Trace
-- The search starts at a random point and proceeds by calculating the costs of a 'normal' size step and a 'large' step,
-- choosing the next candidate within the bounds of the step size around the point.
-- If either candidate has a better cost than the current point, the two costs are compared and the recursion proceeds
-- with the new better solution and, if the 'large' step is better, the new large step size.
-- In the case that neither step is better, a count is kept of the 'no change' calls and the step size is reduced again
-- if the count is exceeded.
aRandomSearch initialStepSize initialSolution iters params randomNums =
	let (min, max, nDims, iterMult, stepFactorLarge, stepFactorSmall, noChangeCountMax, costFn) = params;
	    makeStep' = makeStep min max;
            calcLargeStep' = calcLargeStep iterMult stepFactorLarge stepFactorSmall;
	    cost' = cost costFn;
	    aRandomSearch' :: Float -> [Float] -> Int -> Int -> [Float] -> Trace -> Trace
	    aRandomSearch' _ _ _ 0 _ solutionTrace = solutionTrace
            aRandomSearch' stepSize currentSolution noChangeCount iter randomNums solutionTrace =
	          let (r1, rest1) = splitAt nDims randomNums;
                      (r2, rest2) = splitAt nDims rest1;
                      s1 = makeStep' currentSolution stepSize r1;
                      largeStepSize = calcLargeStep' stepSize iter;
                      s2 = makeStep' currentSolution largeStepSize r2;
                      cc = cost' currentSolution;
                      c1 = cost' s1;
                      c2 = cost' s2
                  in if c1 < cc || c2 < cc
                     then if c1 < c2
                          then aRandomSearch' stepSize s1 0 (iter - 1) rest2 ((iter, c1, stepSize, s1) : solutionTrace)
                          else aRandomSearch' largeStepSize s2 0 (iter - 1) rest2
					      ((iter, c2, largeStepSize, s2) : solutionTrace)
                     else if noChangeCount > noChangeCountMax
                          then let newStepSize = (stepSize / stepFactorSmall)
                               in aRandomSearch' newStepSize currentSolution 0 (iter - 1) rest2 
						 ((iter, cc, newStepSize, currentSolution) : solutionTrace)
                          else aRandomSearch' stepSize currentSolution (noChangeCount + 1) (iter - 1) rest2 solutionTrace
          in aRandomSearch' initialStepSize initialSolution 0 iters randomNums []

makeStep :: Float -> Float -> [Float] -> Float -> [Float] -> [Float]
-- Make a 'normal' step from the current solution bounded by the step size and by
-- the absolute bounds of the solution space.
makeStep min max solution stepSize randoms = 
	let step :: Float -> Float -> Float -> Float
            step x r stepSize = 
              let nmin = maximum [x - stepSize, min];
                  nmax = minimum [x + stepSize, max]
              in nmin + (nmax - nmin) * r
	in [ let (x, r) = z in step x r stepSize | z <- zip solution randoms ]

calcLargeStep :: Int -> Float -> Float -> Float -> Int -> Float
-- Calculate the size of a 'large' step by mutiplying the current step size by
-- the 'small' factor, unless the iteration number is a multiple of the iteration multiplier
-- in which case, use the 'large' factor
calcLargeStep iterMult stepFactorLarge stepFactorSmall stepSize iter =
	stepSize * (if iter > 0 && iter `mod` iterMult == 0
                    then stepFactorLarge
                    else stepFactorSmall)

main seed iters = do
              putStr "The best solution found is: ";
              let nDims = 2;
                  min = (-5.0);
                  max = 5.0;
                  iterMult = 10;
                  stepFactorLarge = 3.0;
                  stepFactorSmall = 1.3;
                  noChangeCountMax = 30;
                  costFn = \n -> n * n;
                  (rs, rest) = splitAt nDims (randomList seed);
                  initialStepSize = 0.03;
                  initialSolution = [ min + (max - min) * x | x <- rs ];
		  params = (min, max, nDims, iterMult, stepFactorLarge, stepFactorSmall, noChangeCountMax, costFn);
                  s = aRandomSearch initialStepSize initialSolution iters params rest
              print s
                  
                  