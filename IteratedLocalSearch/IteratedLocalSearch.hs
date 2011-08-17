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

randomInt :: [Float] -> Int -> Int -> (Int, [Float])
randomInt randNums lo hi = let [r1] = take 1 randNums
                             in ((floor (fromIntegral(hi - lo) * r1)) + lo, (drop 1 randNums))

-- an XY coordinate
type Point = (Float, Float)
square :: Float -> Float
square n = n * n

euclid2d :: Point -> Point -> Float
euclid2d (x1, y1) (x2, y2) = sqrt(square(x1 - x2) + square(y1 - y2))

permutationPairs :: [Int] -> [(Int, Int)]
permutationPairs list = zip list ((drop 1 list) ++ (take 1 list))

cost :: [Int] -> [Point] -> Float
cost tour tsp = let pairs = zip tour ((drop 1 tour) ++ (drop ((length tour) - 1) tour))
                    list = [euclid2d (tsp!!p1) (tsp!!p2) | (p1, p2) <- pairs]
                in foldr (+) 0 list

makeInitialPermutation :: [Point] -> [Int]
makeInitialPermutation cities = [0..((length cities) - 1)]

randomPermutation :: [Int] -> [Float] -> ([Int], [Float])
randomPermutation [last] randNums = ([last], randNums)
randomPermutation (first:rest) randNums =
	let [r] = take 1 randNums
	    len = (length rest) - 1
	    pos = round ((fromIntegral len) * r)
            (perm, newRandNums) = randomPermutation ((take pos rest) ++ [first] ++ (drop (pos + 1) rest))  (drop 1 randNums)
	in  ((rest !! pos) : perm, newRandNums)


reverseSlice :: [Int] -> Int -> Int -> [Int]
reverseSlice list c1 c2 = let reverseSlice1 list c1 c2 
				| c1 > c2 = []
				| otherwise = list!!c1 : reverseSlice1 list (c1 + 1) c2
                          in reverse (reverseSlice1 list c1 c2)

excludeC2 :: [Int] -> Int -> Int -> [Float] -> (Int, [Float])
excludeC2 list c2 l randNums = if elem c2 list then
                                  let (n, newRandNums) = randomInt randNums 0 l
                                  in excludeC2 list n l newRandNums
                               else (c2, randNums)

stochasticTwoOpt :: [Int] -> [Float] -> ([Int], [Float])
stochasticTwoOpt list randNums = let l = length list
				     (c1, newRandNums) = randomInt randNums 0 l
                                     exclude = [c1] ++ [if c1 == 0 then l - 1 else c1 - 1] ++ [if c1 == l - 1 then 0 else c1 + 1]
                                     (n, newRandNums1) = randomInt newRandNums 0 l
                                     (c2, newRandNums2) = excludeC2 exclude n l newRandNums1
                                     n1 = if c2 < c1 then c2 else c1
                                     n2 = if c2 < c1 then c1 else c2
                                 in
				     ((take n1 list) ++ (reverseSlice list n1 n2) ++ (drop (n2 + 1) list), newRandNums2)
                                      
localSearch :: [Int] -> [Point] -> Int -> [Float] -> ([Int], [Float])
localSearch best cities maxNoImprov randNums = let localSearch1 :: Int -> [Int] -> Float -> [Point] -> Int -> [Float] -> ([Int], [Float])
		                                   localSearch1 count best bestCost cities maxNoImprov randNums =
							let (candidate, newRandNums) = stochasticTwoOpt best randNums
                                                            candidateCost = cost candidate cities
                                                        in if count >= maxNoImprov then (best, newRandNums)
                                                           else if candidateCost < bestCost
                                                                then localSearch1 0 candidate candidateCost cities maxNoImprov newRandNums
                                                                else localSearch1 (count + 1) best bestCost cities maxNoImprov newRandNums
                                               in localSearch1 0 best (cost best cities) cities maxNoImprov randNums

doubleBridge :: [Int] -> [Float] -> ([Int], [Float])
doubleBridge list randNums = let quart = floor (fromIntegral(length list) / 4)
                                 (n1, newRandNums1) = randomInt randNums 0 quart
                                 pos1 = 1 + n1
                                 (n2, newRandNums2) = randomInt newRandNums1 0 quart
                                 pos2 = pos1 + 1 + n2
                                 (n3, newRandNums3) = randomInt newRandNums2 0 quart
                                 pos3 = pos2 + 1 + n3
                             in ((take pos1 list ) ++ (drop pos3 list) ++ (take (pos3 - pos2) (drop pos2 list)) ++ (take (pos2 - pos1) (drop pos1 list)),
                                 newRandNums3)

search :: [Point] -> [Int] -> Float -> Int -> Int -> Int -> [Float] -> [([Int], Float)] -> [([Int], Float)]
search cities best bestCost maxIters iter maxNoImprov randNums costHistory
	| iter > maxIters = reverse costHistory
        | otherwise = let (candidate, newRandNums) = doubleBridge best randNums
                          (newCandidate, newRandNums1) = localSearch candidate cities maxNoImprov newRandNums
                          newCost = cost newCandidate cities
                      in if newCost < bestCost
                         then search cities newCandidate newCost maxIters (iter + 1) maxNoImprov newRandNums1 ((newCandidate, newCost) : costHistory)
                         else search cities best bestCost maxIters (iter + 1) maxNoImprov newRandNums1 costHistory

berlin52 :: [Point]
berlin52 = [(565,575),(25,185),(345,750),(945,685),(845,655),
			(880,660),(25,230),(525,1000),(580,1175),(650,1130),(1605,620),
			(1220,580),(1465,200),(1530,5),(845,680),(725,370),(145,665),
			(415,635),(510,875),(560,365),(300,465),(520,585),(480,415),
			(835,625),(975,580),(1215,245),(1320,315),(1250,400),(660,180),
			(410,250),(420,555),(575,665),(1150,1160),(700,580),(685,595),
			(685,610),(770,610),(795,645),(720,635),(760,650),(475,960),
			(95,260),(875,920),(700,500),(555,815),(830,485),(1170,65),
			(830,610),(605,625),(595,360),(1340,725),(1740,245)]

trial1 :: [Point]
trial1 = [(200,850),(850,900),(850,450),(300,200)]

main :: [Point] -> Int -> Int -> Int -> [([Int], Float)]
main tsp maxIters maxNoImprov  seed = let randNums = randomList seed
                                          (first, newRandNums) = randomPermutation (makeInitialPermutation tsp) randNums
                                          firstCost = cost first tsp
                                      in search tsp first firstCost maxIters 0 maxNoImprov newRandNums [(first, firstCost)]


           

-- tester for random permutations
permTest :: [Int] -> [Float] -> Int -> [[Int]]
permTest _ _ 0 = []
permTest list randNums iters = let (newlist, newRandNums) = randomPermutation list randNums in
                                     newlist : permTest newlist newRandNums (iters - 1)

-- tester for stochasticTwoOpt
s2optTest :: [Int] -> [Point] -> [Float] -> Int -> Float -> [([Int], Float)] -> ([([Int], Float)], Float)
s2optTest _ _ _ 0 minCost perms = (perms, minCost)
s2optTest perm cities rs count minCost perms = let (newPerm, newrs) = stochasticTwoOpt perm rs
                                                   newCost = cost newPerm cities 
                                               in s2optTest newPerm cities newrs (count - 1) (min newCost minCost) ((newPerm, newCost) : perms)

-- doubelBridge test
dbTest :: [Int] -> [Float] -> Int -> [[Int]] -> [[Int]]
dbTest _ _ 0 perms = reverse perms
dbTest perm rs count perms = let (newPerm, newrs) = doubleBridge perm rs
                                  in dbTest newPerm newrs (count - 1) (newPerm : perms)
                                                        



