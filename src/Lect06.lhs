% CS 340: Programming Paradigms and Patterns
% Lect 06 - Recursion
% Michael Lee

\begin{code}
module Lect06 where
import Debug.Trace
import qualified Data.Set as Set
\end{code}

Recursion
=========

Agenda:

  - Some common patterns of recursion:
     A. Iteration & Reduction
     B. Filtering
     C. Combinations & Permutations
     D. Divide & Conquer
     E. Tail recursion & Accumulation
  - How to trace and debug in Haskell
  - How laziness affects recursive call evaluation


A. Iteration & Reduction

Iteration is the process of repeatedly applying a function to a value
until one or more conditions (base cases) are met. It often makes sense to 
think of iteration as incrementally "building up" a result, as in constructing 
a list element by element. Sometimes, iteration is used to "reduce" an input to a final value (e.g., as in summing up the elements of a list).

E.g., implement the following functions using iteration/reduction:

\begin{code}
-- a classic!
factorial :: Integer -> Integer
factorial 1 = 1
factorial n = traceShow n (n * factorial (n-1))


-- sum up the elements of a list
sumList :: (Show a, Num a) => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs
{- 
sumList 1:2:3:[]
1 + sumList 2:3:[]
1 + (2 + sumList 3:[])
1 + (2 + (3 + sumList []))
1 + (2 + (3 + 0))
1 + (2 + 3)
1 + 5
6
-}


-- sometimes we iterate over lists in parallel
weightedSum :: (Show a, Num a) => [a] -> [a] -> a
weightedSum [] [] = 0
weightedSum (v:vs) (w:ws) = v * w + weightedSum vs ws


-- sometimes we process more than one "item" at a time
swapLetters :: String -> String
swapLetters "" = ""
swapLetters [x] = [x]
swapLetters (x:y:xs) = y : x : swapLetters xs


-- implement this using append (++)
cycle' :: [a] -> [a]
cycle' l = l ++ cycle' l


-- can we do better? (why is it better?)
cycle'' :: [a] -> [a]
cycle'' l = cyc l
  where cyc [] = cyc l 
        cyc (x:xs) = x : cyc xs


-- we'll need to pass values into subsequent iterations to track progress
-- 0 1 1 2 3 5 8 13 ...
fibs :: [Integer]
fibs = f 0 1
  where f f0 f1 = f0 : f f1 (f0+f1)
\end{code}

fibs = f 0 1
f 0 1 = 0 : f 1 1
        0 : 1 : f 1 2
        0 : 1 : 1 : f 2 3
        0 : 1 : 1 : 2 : f 3 5


B. Filtering (conditional iteration/reduction)

Filtering is the process of iterating over a list and processing only those elements that satisfy a given condition. 

\begin{code}
-- sum only the positive numbers in a list
sumPositives :: Integral a => [a] -> a
sumPositives [] =  0
sumPositives (x:xs) = (if x > 0 then (x+) else id)  (sumPositives xs)


-- palindroms are strings that read the same forwards as backwards
palindromes :: [String] -> [String]
palindromes [] = []
palindromes (w:ws) = (if w == reverse w then (w:) else id) (palindromes ws)
\end{code}


C. Combinations & Permutations

Combinations and permutations are classic problems in combinatorics that arise 
in many different problems.

\begin{code}
-- generate all combinations (order doesn't matter -- how many are there?)
-- e.g., combinations [1, 2, 3]
--   [1,2,3], [1,2], [1,3], [1], [2,3], [2], [3], []
combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = [ x:ys | ys <- combinations xs ] ++ combinations xs


-- generate all combinations of a given size (nCr = n!/(r!(n-r)!))
combinations' :: Int -> [a] -> [[a]]
combinations' 0 _ = [[]]
combinations' _ [] = []
combinations' n (x:xs) = [ x:ys | ys <- combinations' (n-1) xs ] ++ combinations' n xs


-- the "making change" problem
change :: (Ord a, Num a) => a -> [a] -> [[a]]
change 0 _ = [[]]
change _ [] = []
change amt den@(d:ds) 
  | amt < d = change amt ds 
  | otherwise = [ d:es | es <- change (amt-d) den] ++ change amt ds


-- the knapsack problem: given a list of items (value,weight) and a weight 
-- capacity, find the maximum value that can be carried
knapsack :: (Ord a, Num a) => a -> [(a,a)] -> a
knapsack _ [] = 0
knapsack wcap ((v,w):is) 
  | w > wcap = knapsack wcap is 
  | otherwise = max (v + knapsack (wcap - w) is) (knapsack wcap is)


-- find the actual set of items that maximizes value (under the weight cap)
knapsack' :: (Ord a, Num a) => a -> [(a,a)] -> [(a,a)]
knapsack' _ [] = []
knapsack' wcap ((v,w):is) 
  | w > wcap = knapsack' wcap is 
  | otherwise = let r1 = knapsack' (wcap - w) is
                    r2 = knapsack' wcap is
                in if v + value r1 > value r2 then (v,w):r1 else r2
  where value [] = 0
        value ((v,w):is) = v + value is


-- find the two closest points in a list of points (brute force)
-- closestPoints [(0,0), (1,5), (2,3), (2,4), (3,8)]
closestPoints :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
closestPoints ps = minOf (combinations' 2 ps)
  where minOf [] = []
        minOf [ps] = ps 
        minOf (ps:pss) = let r = minOf pss 
                         in if dist ps < dist r then ps else r
        dist [(x1,y1),(x2,y2)] = (x1-x2)^2 + (y1-y2)^2


-- generate all permutations (order matters -- how many are there?)
-- e.g. permutations [1,2] = 1,2; 2,1
-- e.g. permutations [1,2,3] = 1,2,3; 1,3,2; 2,1,3; 
--                             3,2,1; 2,3,1; 3,1,2
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat [interleave x zs | zs <- permutations xs]
  where interleave x [] = [[x]]
        interleave x (y:ys) = (x:y:ys) : [y:zs | zs <- interleave x ys]


-- generate all palindromes from a given string
allPalindromes :: String -> [String]
allPalindromes w = [ w' | w' <- permutations w, w' == reverse w' ]
\end{code}


D. Divide & Conquer

Divide and conquer is a technique for solving problems by breaking them into
smaller subproblems and then combining the solutions to the subproblems to
obtain a solution to the original problem.

\begin{code}
-- a classic!
fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- sort by splitting the list in half and merging the sorted halves
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort [x,y] = if x < y then [x,y] else [y,x]
mergesort xs = let (ys,zs) = splitAt (length xs `div` 2) xs 
               in merge (mergesort ys) (mergesort zs)
  where merge xs [] = xs 
        merge [] ys = ys
        merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                            | otherwise = y : merge (x:xs) ys


-- sort by choosing a pivot and "partitioning" the list around it
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort [x,y] = if x < y then [x,y] else [y,x]
quicksort (x:xs) = quicksort [ x'  | x' <- xs, x' < x] 
                   ++ [x] ++
                   quicksort [ x'' | x'' <- xs, x'' > x]


-- find the two closest points in a list of points (more efficiently)
closestPoints' :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
closestPoints' = undefined
\end{code}


E. Tail recursion & Accumulation

Tail recursion is a special case of recursion where the recursive call is the
last thing done in the function.  In non-lazy languages, this is important
because it allows the compiler to optimize the code by eliminating the need
for a stack frame. In Haskell (and other lazy languages), tail recursion does 
not quite have the same importance, but it is still a useful technique.

Accumulation is a technique for solving problems by passing an extra
parameter to the recursive call that accumulates the solution.

\begin{code}
-- are all elements even?
allEven :: [Integer] -> Bool
allEven [] = True
-- allEven (x:xs) = even x && allEven xs
-- allEven (x:xs) = if even x then allEven xs else False
allEven (x:xs) | even x = allEven xs 
               | otherwise = False


-- are two lists the same length?
sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength _  [] = False 
sameLength [] _  = False 
sameLength (_:xs) (_:ys) = sameLength xs ys


-- tail recursive factorial with explicit accumulator
factorial' :: Integer -> Integer -> Integer
factorial' 1 acc = acc
factorial' n acc = factorial' (n-1) (acc * n)
{-
factorial 10 1
factorial  9 (1 * 10)
factorial  8 ((1 * 10) * 9)
factorial  7 (((1 * 10) * 9) * 8)
factorial  6 ((((1 * 10) * 9) * 8) * 7)
...
-}


-- tail recursive factorial with hidden accumulator
factorial'' :: Integer -> Integer
factorial'' n = f 1 1
  where f m acc | m == n = acc * n
                | otherwise = traceShow acc (f (m+1) (acc*m))


-- reverse a list using an accumulator
reverse' :: Show a => [a] -> [a]
-- reverse' [] = []
-- reverse' (x:xs) = reverse' xs ++ [x]
reverse' l = rev l [] 
  where rev [] acc = acc 
        rev l@(x:xs) acc = traceShow (l,acc) (rev xs (x:acc))


-- enumerate the integers from m to n (with an accumulator)
enumFromTo' :: Integer -> Integer -> [Integer]
-- enumFromTo' m n | m > n = []
--                 | m == n = [m]
--                 | otherwise = m : enumFromTo' (m+1) n
-- enumFromTo' m n = eft m []
--   where eft i acc | i == n = n:acc 
--                   | otherwise = eft (i+1) (i:acc)
enumFromTo' m n = eft n []
   where eft i acc | i == m = m:acc 
                   | otherwise = eft (i-1) (i:acc)


-- can we write the infinite list version using an accumulator?
enumFrom' :: Integer -> [Integer]
-- enumFrom' n = n : enumFrom' (n+1)
enumFrom' n = ef n []
  where ef i acc = ef (i+1) (i:acc)
\end{code}
