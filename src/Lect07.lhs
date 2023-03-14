% CS 340: Programming Paradigms and Patterns
% Lect 07 - Higher order functions
% Michael Lee

\begin{code}
module Lect07 where
import Prelude hiding (($), (.), flip, on, and,
                       map, filter, any, all, iterate, until,
                       foldr, foldl, foldr1, foldl1)
import Data.Char
import Data.Bits ( Bits(xor) )
import Data.Function hiding (($), (.), flip, on)
import Data.List (minimumBy)
import Debug.Trace
\end{code}

Higher order functions
======================

Agenda:
  - HOFs and Combinators
  - Basic combinators
  - Recursive patterns via HOFs
  - Bonus HOFs


HOFs and Combinators
--------------------

A higher-order function (HOF) is a function that takes a function as a parameter or returns a function. (Non-HOFs are called first-order functions).
They are a fundamental tool in functional programming.

The term "combinator" is often used to refer to HOFs that combine or apply argument functions to do all their work.


Basic combinators
-----------------

1. Application:

\begin{code}
($) :: (a -> b) -> a -> b
infixr 0 $
f $ x = f x
\end{code}

It seems redundant (why?), but is quite useful in practice!

E.g., how can we rewrite the following expresions?

\begin{verbatim}
  putStrLn ("Hello" ++ " " ++ "World")

  show (abs (2 - 5))

  take 5 (drop 10 (zip [1..] (repeat 'a')))
\end{verbatim}


2. Composition

\begin{code}
(.) :: (b -> c) -> (a -> b) -> a -> c
infixr 9 .
f . g = \x -> f $ g x
\end{code}    

E.g., re-implement `even'`, `k2h`, and `strip` with composition:

\begin{code}
even' :: Integral a => a -> Bool
-- even' x = 0 == (x `rem` 2)
even' = (== 0) . (`rem` 2)  -- e.g., a "point-free" function definition
k2c :: Num a => a -> a
k2c k = k - 273

c2f :: Fractional a => a -> a
c2f c = c * 9 / 5 + 32

f2h :: (Ord a, Num a) => a -> String
f2h f
  | f < 0     = "too cold"
  | f > 100   = "too hot"
  | otherwise = "survivable"

k2h :: (Ord a, Fractional a) => a -> String
-- k2h  k = f2h $ c2f $ k2c k
k2h = f2h . c2f . k2c

strip :: String -> String
-- strip s = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
-- This gets rid of the spaces at the beginning and the end of a list
-- strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
--          OR
strip = let f = reverse . dropWhile isSpace in f . f
\end{code}


3. Flip, On, and (many) others

Combinators are especially useful when paired with other HOFs!

\begin{code}
flip :: (a -> b -> c) -> b -> a -> c
-- flip f = \x y -> f y x 
--          OR
flip f x y = f y x

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on f g = \x y -> f (g x) (g y)
--          OR
on f g x y = f (g x) (g y)
\end{code}


Recursive patterns via HOFs
---------------------------

1. Map: apply a function to each item of a list, returning the new list.

\begin{code}
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
\end{code}


E.g.,

\begin{verbatim}
  map (^2) [1..10]

  take 10 $ map (^2) [1..]

  map reverse $ words "madam I refer to adam"

  map (\x -> (x,x^2)) [1..10]

  map (++) $ words "on over under across"

  map ($ " the sea") $ map (++) $ words "on over under across"

  map ($ "jump ") $ map (flip (++)) $ words "on over under across"

  map (map (*2)) [[1..5], [6..10], [11..15]]
\end{verbatim}



2. Filter: keep only the elements of a list that satisfy a predicate.

\begin{code}
filter :: (a -> Bool) -> [a] -> [a]
filter _  [] = []
filter p (x:xs) | p x = x : filter p xs 
                | otherwise = filter p xs
\end{code}                 


E.g.,

\begin{verbatim}
  filter even [1..10]

  take 10 $ filter even [1..]

  filter (\(a,b,c) -> a^2+b^2 == c^2) $
         [(a,b,c) | a <- [1..10], b <- [a..10], c <- [b..10]]

  filter (\s -> reverse s == s) $ 
         words "madam I refer to adam"

  map (\w -> (w,length w)) $ 
      filter (\s -> reverse s == s) $ 
             words "madam I refer to adam"
\end{verbatim}  


3. All & Any

\begin{code}
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all p (x:xs) = p x && all p xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any p (x:xs) = p x || any p xs
\end{code}

E.g.,

\begin{verbatim}
  all even [2,4..10]

  any even [1..]

  filter (any isDigit) $ words "hello 123 world a456b"
\end{verbatim}  


4. HOF versions of sort (and others)

\begin{code}
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [y | y <- xs, y < x] 
              ++ [x] 
              ++ sort [y | y <- xs, y >= x]


sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:xs) = sortBy cmp [y | y <- xs, cmp x y == LT] 
                    ++ [x] 
                    ++ sortBy cmp [y | y <- xs, cmp x y /= LT]
\end{code}

E.g.,
\begin{verbatim}
  sortBy (flip compare) [1..10]

  sortBy (compare `on` length) $ words "madam I refer to adam"

  minimumBy (compare `on` length) $ words "madam I refer to adam"
\end{verbatim}  


5. Fold

Consider the recursive patterns found in:

\begin{code}
and :: [Bool] -> Bool
and [] = True
and (x:xs) = (&&) x $ and xs


showCat :: Show a => [a] -> String
showCat [] = ""
showCat (x:xs) = ((++) . show) x $ showCat xs
\end{code}


What is the essential pattern here?

- the input tyoe is a list, [a]

- the return type is something else, b 

- i.e., we have [a] -> b

- the base case returns some fixed value of type b

- the recursive case applies a "combiner function" to the first element of
  the list (of type a), and then to the return value of th erecursive call (b) 

  - i.e., the combiner function has type: a -> b -> b 

Write the HOF that captures this pattern:

\begin{code}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ val [] = val
foldr f val (x:xs) = f x $ foldr f val xs
\end{code}


E.g., trace the evaluation of `foldr (+) 0 [1..5]`:

  foldr (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

= 1 + (foldr (+) 0 (2 : (3 : (4 : (5 : [])))))

= 1 + (2 + (foldr (+) 0 (3 : (4 : (5 : [])))))

= 1 + (2 + (3 + (foldr (+) 0 (4 : (5 : [])))))

= 1 + (2 + (3 + (4 + (foldr (+) 0 (5 : [])))))

= 1 + (2 + (3 + (4 + (5 + (foldr (+) 0 [])))))

= 1 + (2 + (3 + (4 + (5 + 0))))

= 1 + (2 + (3 + (4 + 5)))

= 1 + (2 + (3 + 9))

= 1 + (2 + 12)

= 1 + 14

= 15

Let's define some recursive functions in terms of foldr:

\begin{code}
and' :: [Bool] -> Bool
and' = foldr (&&) True

showCat' :: Show a => [a] -> String
showCat' = foldr ((++) . show) ""

(+++) :: [a] -> [a] -> [a]
l1 +++ l2 = foldr (:) l2 l1

length' :: [a] -> Int
length' = foldr (\_ l -> l + 1) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr f []
  where f x r | p x = x : r 
              | otherwise = r
\end{code}


Consider this traced version of `foldr`:

\begin{code}
foldrT :: (Show a, Show b) => (a -> b -> b) -> b -> [a] -> b
foldrT _ v [] = v
foldrT f v (x:xs) = let e  = trace ("<" ++ show x ++ ">") x
                    in trace "R" $ f e $ foldrT f v xs
\end{code}


Experiment with the following to answer these questions:

- In what order are the input list elements evaluated?

- When are the combining functions applied?

- Does the right fold work on infinite lists? Why or why not?

- Is the intuition that the right fold "replaces" the empty list with the
  base case value correct? Why or why not?

\begin{verbatim}
foldrT (+) 0 [1..10]

foldrT (&&) True [True, False, True, False]

foldrT (&&) undefined $ repeat False

take 5 $ foldrT (:) [] [1..]

take 5 $ foldrT ((++) . show) "" [1..]
\end{verbatim}


-------------------------------------------------------------------------------


Consider the recursive patterns found in:

\begin{code}
hash :: Integer -> String -> Integer
hash seed [] = seed
hash seed (c:cs) = hash (h seed c) cs
  where h v c = (7*v `xor` fromIntegral (ord c)) `mod` 1000007


playMoves :: [Char] -> [(Int,Char)] -> [Char]
playMoves board [] = board
playMoves board (m:moves) = playMoves (move board m) moves
  where move board (x,y) = take x board ++ [y] ++ drop (x+1) board
\end{code}


How is the pattern different from before?

- the recursive call is in the tail position

- the base case returns the accumulated value

- the combiner function takes the accumulator and an element and returns
  a new result (this result is passed into the recursive call)

Write the HOF that captures this pattern:

\begin{code}
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc 
foldl f acc (x:xs) = foldl f (f acc x) xs
\end{code}


E.g., trace the evaluation of `foldl (+) 0 [1..5]`:

  foldl (+) 0 (1 : (2 : (3 : (4 : (5 : [])))))

= foldl (+) (0 + 1) (2 : (3 : (4 : (5 : []))))

= foldl (+) ((0 + 1) + 2) (3 : (4 : (5 : [])))

= foldl (+) (((0 + 1) + 2) + 3) (4 : (5 : []))

= foldl (+) ((((0 + 1) + 2) + 3) + 4) (5 : [])

= foldl (+) (((((0 + 1) + 2) + 3) + 4) + 5) []

= (((((0 + 1) + 2) + 3) + 4) + 5) -- <-- a "thunk" (a big unevaluated function)

= ((((1 + 2) + 3) + 4) + 5)

= (((3 + 3) + 4) + 5)

= ((6 + 4) + 5)

= (10 + 15)

= 15


Let's define some recursive functions in terms of foldl:

\begin{code}
hash' :: String -> Integer
hash' = foldl h 107
  where h acc c = (7*acc `xor` fromIntegral (ord c)) `mod` 1000007

playMoves' :: [(Int,Char)] -> [Char]
playMoves' = foldl move "-------------" 
  where move board (x,y) = take x board ++ [y] ++ drop (x+1) board
\end{code}


Consider this traced version of `foldl`:

\begin{code}
foldlT :: (Show a, Show b) => (b -> a -> b) -> b -> [a] -> b
foldlT _ v [] = v
foldlT f v (x:xs) = let e  = trace ("<" ++ show x ++ ">") x
                        a  = f v e
                        a' = trace ("<<" ++ show a ++ ">>") a
                    in trace "R" $ foldlT f a' xs
\end{code}


Experiment with the following to answer these questions:

- In what order are the input list elements evaluated?

- When is the combining function applied?

- Does the left fold work on infinite lists? Why or why not?

- How might we make the left fold more efficient?

\begin{verbatim}
foldlT (+) 0 [1..10]

foldlT (&&) True [True, False, True, False]

foldlT (&&) True $ repeat False

take 3 $ foldlT (flip (:)) [] [1..10]
\end{verbatim}


We can force Haskell to be stricter by using `seq`, which has type:

    seq :: a -> b -> b

`seq` takes two arguments and forces strict evaluation of its first argument
before evaluating the second argument (and returning a result). 


Write a stricter (traced) version of the left fold:

\begin{code}
foldlTS :: (Show a, Show b) => (b -> a -> b) -> b -> [a] -> b
foldlTS _ v [] = v
foldlTS f v (x:xs) = let e  = trace ("<" ++ show x ++ ">") x
                         a  = f v e
                         a' = trace ("<<" ++ show a ++ ">>") a
                     in trace "R" $ a' `seq` foldlTS f a' xs
\end{code}

E.g., try `foldlTS (+) 0 [1..10]` and `foldlTS (flip (:)) [] [1..10]`

How is this more efficient than the previous version? When is it arguably better than the right fold?


-------------------------------------------------------------------------------

When to fold left or right?

-- Usually, the right fold is better. 
  - When the list is infinite

  - When the combining function can short-circuit (i.e., it isn't
    strict in both arguments)

  - When the combining function is right associative

  - When we can't tolerate a reversal of elements when constructing the result
  
- Left (Strict), when:

  - The function is left associative (and the list isn't infinite)

  - The function is commutative (or left-associative), and the entire
    list needs to be processed to get the result --- this takes advantage of
    the accumulator pattern

- Very, very, very rarely is the non-strict left fold the right one


Bonus HOFs
----------

It is convenient to have folds that use the first element of the list as the
initial value (and thus don't require an initial value to be passed in).

E.g.,

\begin{code}
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x $ foldr1 f xs
-- e.g., foldr1 (*) [1..5]
--       foldr1 (^) [2,2,3]


foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs) = foldl f x xs
-- e.g., foldl1 (++) [[1,2], [3,4], [5,6]]
--       foldl1 (/) [16,2,4]
\end{code}


The `scan` variants are like fold, but they return a list of the intermediate
values of the result/accumulator.

E.g., try:

\begin{verbatim}
  scanr (+) 0 [1..5]

  scanl (+) 0 [1..5]

  scanr1 (*) [1..5]

  scanl1 (++) [[1,2], [3,4], [5,6]]
\end{verbatim}


`iterate` takes a function and an initial value, and returns an infinite list
of repeated applications of the function to the initial value.

\begin{code}
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
-- e.g., take 10 $ iterate (*2) 1
\end{code}


`until` takes a predicate and a function, and returns the first value that
satisfies the predicate when repeatedly applied to the function.

\begin{code}
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x = x 
            | otherwise = until p f (f x)
-- e.g., until (> 100) (*2) 1
-- e.g., let n = 2 in until (\x -> abs (n-x*x) < 0.001) (\x -> (x + n/x) / 2) 1
\end{code}
