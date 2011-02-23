-----------------------------------------------------------------
--                                                             --
--	StochasticHillClimbing.hs                              --
--                                                             --
--	This implements a sub-optimal random search in         --
--	a bounded n-dimensional space of floating-point values --
--	where random steps are taken when they improve the     --
--      cost                                                   --                                        --
--                                                             --
-----------------------------------------------------------------

module StochasticHillClimbing where

-- the random library: randoms is an infinite list of random numbers between 0 and 1
import System.Random

randomList :: Int -> [Float]
-- make an RNG using the seed and return the infinite list of randoms
randomList seed = randoms (mkStdGen seed)

type BinaryVector = [Int]

type Trace = [BinaryVector]

oneMax :: [Int] -> Int
-- sum the number of 1s in the list
oneMax lst = foldr1 (+) lst

flp :: Int -> Int
flp b = if b == 0 then 1 else 0

randomNeighbor :: BinaryVector -> [Float] -> (BinaryVector, [Float])
-- flip a random bit in the input solution
randomNeighbor solution randNums =
	let [r] = take 1 randNums
	    len = length solution
	    pos = round ((fromIntegral len) * r)
	in ([ if n == pos then flp (solution !! n) else (solution !! n) | n <- [0, 1 .. (len - 1)]],
	    drop 1 randNums)

hillClimb :: BinaryVector -> Int -> Int -> [Float] -> Trace -> (Trace, Int)
-- The search starts with a random vector of 1s and 0s and proceeds by taking a random step by flipping one bit.
-- The cost function is just the sum of the bits in the current solution.
-- If the new candidate has a better cost than the current solution, it becomes the current best solution.
-- If this solution is all 1s, the algorithm stops.
hillClimb _ 0 n _ trace= (trace, n)
hillClimb solution iter max randNums trace
	| oneMax solution == length solution = (trace, max - iter + 1)
	| otherwise = let (candidate, rs) = randomNeighbor solution randNums
	   	      in if (oneMax candidate) > (oneMax solution)
		         then hillClimb candidate (iter - 1) max rs (trace ++ [candidate])
		         else hillClimb solution (iter - 1) max rs trace

printList :: ([BinaryVector], Int) -> IO ()
printList ([], n) = do putStr "Iterations:"; print n
printList ((v:vs), n) = do print v; printList (vs, n)	

main numBits iters seed = do
              let (rs, rest) = splitAt numBits (randomList seed);
                  initialSolution = [ round x | x <- rs ];
                  s = hillClimb initialSolution iters iters rest [initialSolution]
              putStrLn "The best solution found is: ";
	      printList s;
                  
                  