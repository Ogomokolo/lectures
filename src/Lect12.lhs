% CS 340: Programming Paradigms and Patterns
% Lect 12 - Search
% Michael Lee

\begin{code}
{-# LANGUAGE FlexibleInstances #-}

module Lect12 where
import Prelude hiding (lines)
import Data.Maybe
import Data.Ord
import Data.List hiding (lines, insert)
import Data.List.Split (chunksOf)
import Data.Tree
import Data.Word
import Data.Map (Map, empty, fromList, keys, elems, assocs,
                 findWithDefault, member, insert, insertWith)
import System.Random
import System.Random.Shuffle
import Control.Monad.State
import System.IO
import System.Console.ANSI
import Control.Concurrent
import GHC.IO
import Debug.Trace
import Data.Array
\end{code}


Search
======

Agenda:
  - Why and How?
  - Maze-building
    - Random values and State
  - Search
    - Uninformed search
    - Informed search
    - A* search

Why and How?
------------

Search is one of the most common programming tasks you'll perform. It's
useful to recognize some common search patterns and techniques, and understand
how they might be applied.


Maze-building
-------------

We need some data types to represent mazes:

\begin{code}
type MazeDims = (Int, Int) -- (width,height)

type MazeLoc = (Int, Int) -- (x,y); (1,1) @ top-left

type MazePath = [MazeLoc] -- path through the maze

data Maze = Maze { 
              mazeDims :: MazeDims, 
              mazePath :: MazePath,
              mazeAdjMap :: Map MazeLoc [MazeLoc] 
            } deriving (Eq)

emptyMaze :: MazeDims -> Maze
emptyMaze d = Maze d [] empty
\end{code}


And to help us visualize the mazes we generate, some drawing functions (which
return ASCII art strings):

\begin{code}
-- draw the north and west borders (either lines for disconnected neighbors
-- or spaces for connected ones) of the specified location; we also draw
-- breadcrumbs for the specified path
drawWallsNW :: MazeLoc -> Maze -> (String, String)
drawWallsNW c@(x, y) (Maze (w, h) p cmap) = 
  let nc = (x, y-1)
      wc = (x-1, y)
      adj = findWithDefault [] c cmap
  in ("+" ++ if nc `elem` adj then "   " else "---",
      (if wc `elem` adj then " " else "|") 
        ++ (if c `elem` p then " o " else "   "))

-- draw the entire maze by assembling the location strings
drawMaze :: Maze -> String
drawMaze m@(Maze (w, h) _ _) = (concat $ map drawRow $ chunksOf w drawnCells) 
                                 ++ bot
  where drawRow cs = let (l1, l2) = unzip cs
                     in concat l1 ++ "+\n" ++ concat l2 ++ "|\n"
        drawnCells = [drawWallsNW (x, y) m | y <- [1..h], x <- [1..w]]
        bot = (concat $ replicate w "+---") ++ "+"

instance Show Maze where
  show = drawMaze        
\end{code}


Next, some utility functions for building up our mazes:

\begin{code}
-- return adjacent (but not necessarily accessible) locations
adjLocs :: MazeDims -> MazeLoc -> [MazeLoc]
adjLocs (w, h) (x, y) = 
  [(x', y') | (dx, dy) <- [(-1,0), (0,-1), (1,0), (0,1)],
              let (x', y') = (x+dx, y+dy),
              x' > 0 && x' <= w,
              y' > 0 && y' <= h]

-- connects two adjacent locations in the maze by inserting them into
-- each others' lists in the adjacency map
openWall :: MazeLoc -> MazeLoc -> Maze -> Maze
openWall l1 l2 mz@(Maze _ _ cmap) = 
  -- l1 -> [l2] , l2 -> [l1] 
  mz { mazeAdjMap = insertWith (++) l2 [l1] $ insertWith (++) l1 [l2] cmap}
\end{code}


-- Random values

To randomly generate mazes, we will use the Pseudo-Random Number Generator (PRNG) interface defined by the class `RandomGen`:

\begin{verbatim}
class RandomGen g where
  genWord16 :: g -> (Word16, g)
  genWord32 :: g -> (Word32, g)
  genWord64 :: g -> (Word64, g)
  split :: g -> (g, g)
\end{verbatim}


The `StdGen` type is an instance of `RandomGen`. We can create a deterministic PRNG using `mkStdGen`, or we can create a PRNG that uses the system clock to seed the PRNG using `newStdGen`:

\begin{verbatim}
instance RandomGen StdGen

mkStdGen :: Int -> StdGen
newStdGen :: IO StdGen
\end{verbatim}


The `Random` class defines a set of methods that can be used to generate random values of other types:

\begin{verbatim}
class Random a where
  randomR  :: RandomGen g => (a, a) -> g -> (a, g)
  random   :: RandomGen g => g -> (a, g)
  randomRs :: RandomGen g => (a, a) -> g -> [a]
  randoms  :: RandomGen g => g -> [a]

instance Random Integer
instance Random Int
instance Random Double
instance Random Char
instance Random Bool
\end{verbatim}


Let's write a function that generates three random integers in a range (using `randomR`):

\begin{code}
threeRandomInts :: RandomGen g => (Int,Int) -> g -> (Int, Int, Int)
threeRandomInts range g = let (v1, g')  = randomR range g
                              (v2, g'') = randomR range g'
                              (v3, _)   = randomR range g''
                          in (v1, v2, v3)
\end{code}

How do we use `threeRandomInts` with `newStdGen`, and what is the result type?

---

Intead of sequencing the `RandomGen` manually, we can use the `State` monad to manage the `RandomGen` for us. As a reminder, here are the definitions of `State` and its `get` and `put` functions:

\begin{verbatim}
newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
\end{verbatim}


Implement the function `getRandom` uses the `RandomGen` in the `State` monad to generate a random value in the specified range:

\begin{code}
getRandom :: (Int, Int) -> State StdGen Int
getRandom range = do g <- get 
                     let (v, g') = randomR range g
                     put g'
                     return v
\end{code}

Now we can use `getRandom` to generate three random integers:

\begin{code}
threeRandomInts' :: (Int,Int) -> State StdGen (Int, Int, Int)
threeRandomInts' range = do v1 <- getRandom range 
                            v2 <- getRandom range 
                            v3 <- getRandom range 
                            return (v1, v2, v3)
\end{code}


There is also a convenient `shuffle'` function that takes a list, its length, and a `RandomGen`, and returns a shuffled version of the list:

\begin{verbatim}
shuffle' :: RandomGen g => [a] -> Int -> g -> [a]
\end{verbatim}


Write a function that uses the `State` monad to shuffle a list:

\begin{code}

getShuffled :: [a] -> State StdGen [a]
getShuffled l = do g <- get 
                   let (g', g'') = split g
                       l' = shuffle' l (length l) g'
                   put g''
                   return l'
\end{code}

---

And now we're ready to implement a random maze generator!

<Discuss> "Recursive backtracking" algorithm:

  1. Pick a starting cell to visit -- this is the "current" cell.

  2. Pick a neighboring cell that has yet to be visited and connect it to the
     current cell. The neighboring cell becomes the new current cell.

  3. Keep repeating (2) until the current cell's neighbors have all been
     visited, then back up to the previous cell and continue. 

  4. All cells should be connected when we back out of the starting cell.


\begin{code}
-- attempt 1: create a maze with a single "tunnel"
genMazeSimple :: MazeDims -> State StdGen Maze
genMazeSimple dims = gen (emptyMaze dims) (1,1) 
  where gen mz@(Maze _ _ cmap) currLoc = do 
          newLocs <- getShuffled $ adjLocs dims currLoc 
          let (newLoc:_) = newLocs
          if newLoc `member` cmap 
          then return mz 
          else gen (openWall currLoc newLoc mz) newLoc

-- maze generator using recursive backtracking
genMaze :: MazeDims -> State StdGen Maze
genMaze dims = gen (emptyMaze dims) (1,1) 
  where gen mz currLoc = do 
          newLocs <- getShuffled $ adjLocs dims currLoc 
          foldM (\mz'@(Maze _ _ cmap) newLoc ->
            if newLoc `member` cmap 
            then return mz' 
            else gen (openWall currLoc newLoc mz') newLoc)
            mz newLocs

-- convenience function for creating a random maze from the global RNG
randomMaze :: MazeDims -> IO Maze
randomMaze dims = evalState (genMaze dims) <$> newStdGen
\end{code}


Search
------

The search problem can be expressed in terms of a search space made up of a
network of interconnected nodes. A search algorithm seeks to locate a goal node
-- i.e., one satisfying certain criteria -- by traversing this network.

A search strategy should avoid re-visiting nodes, and address the following:

  - if we have multiple nodes under consideration, which do we visit first? 

  - if we discover additional traversable nodes, how do we combine those with
    existing, as yet unvisited nodes?


<Discuss> Search helper functions (nodes of type `a`):

  - is a given node the goal node?

    goal :: a -> Bool
  

  - what node(s) are adjacent to / reachable from the given one?

    adj/succ :: a -> [a]


  - how do we combine newly discovered nodes with other unvisited ones?

    comb :: [a] -> [a] -> [a]



\begin{code}
-- generalized, higher-order search
search :: (Eq a, Show a) => 
          (a -> Bool) 
          -> (a -> [a]) 
          -> ([a] -> [a] -> [a])
          -> [a] -> [a] 
          -> Maybe a
search goal adj comb unvisited visited
  -- case unvisited of
  --   [] -> Nothing
  --   (x:xs) -> if goal x 
  --             then Just x 
  --             else search goal adj comb (nub (comb xs (adj x))) (x:visited)
  | null unvisited = Nothing 
  | goal (head unvisited) = Just $ head unvisited 
  | otherwise = debug (head unvisited) $ search goal adj comb 
    (nub (adj $ head unvisited) `comb` tail unvisited)
    (head unvisited : visited) 

searchNumTree :: Integer -> Maybe Integer 
-- searchNumTree n = search goal adj comb [1] [] 
--   where goal x = x == n 
--         adj x = [2*x, 2*x+1] 
--         comb = (++) 
-- This implementation gives us the DFS so to fix it, we need to flip the comb args
-- searchNumTree n = search (== n) (\m -> [2*m, 2*m+1]) (++) [1] []
searchNumTree n = search (== n) (\m -> [2*m, 2*m+1]) (flip (++)) [1] []

-- convenience function for tracing search execution
debug :: Show a => a -> b -> b
debug x y = unsafePerformIO clearScreen `seq`
            unsafePerformIO (setCursorPosition 0 0) `seq`
            unsafePerformIO (putStrLn $ show x) `seq`
            unsafePerformIO (threadDelay $ 3*10^5) `seq`
            y
\end{code}


-- Uninformed search

<Discuss> Uninformed / "brute force" search strategies

<Discuss> Depth-first (LIFO) & Breadth-first search (FIFO)

\begin{code}
dfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
dfs goal succ start = search goal succ (++) [start] []

bfs :: (Eq a, Show a) => (a -> Bool) -> (a -> [a]) -> a -> Maybe a
bfs goal succ start = search goal succ (flip (++)) [start] []
\end{code}


Let's solve our maze using uninformed search:

\begin{code}
solveMaze :: Maze -> Maybe Maze
solveMaze mz@(Maze (w,h) _ _) = 
  dfs (\(Maze _ path@((x,y):_) _) -> (x == w && y == h)) 
      nextPaths  
      (mz {mazePath = [(1,1)]})

-- given a maze with a non-empty path, return a list of mazes, each of
-- which extends the path by one location (based on the adjacency map)
nextPaths :: Maze -> [Maze]
nextPaths mz@(Maze dims p@(loc:_) cmap) = do
  adj <- filter (not . flip elem p) $ findWithDefault [] loc cmap
  return $ mz { mazePath = adj:p }

-- maze for testing nextPaths
testMaze1 = openWall (1,1) (2,1) 
            $ openWall (1,1) (1,2)
            $ openWall (1,2) (2,2)
            $ Maze (2,2) [(1,1)] empty

-- convenience function for solving a maze given dimensions and a solver
solveRandomMaze :: MazeDims -> (Maze -> Maybe Maze) -> IO Maze
solveRandomMaze dims solver = do mz <- randomMaze dims
                                 return $ fromJust $ solver mz
\end{code}

---

Besides order of discovery, we can also rank unvisited nodes using some
cost function:

  cost :: Ord b => a -> b

    - returns an orderable value representing the cost for a node of type `a`

When considering multiple unvisited nodes, the "best-first" search strategy
chooses the node with lowest cost:

\begin{code}
bestFirstSearch :: (Eq a, Show a, Ord b) => 
                   (a -> Bool) 
                   -> (a -> [a])
                   -> (a -> b) 
                   -> a -> Maybe a
bestFirstSearch goal succ cost start = 
  search goal succ (\xs ys -> sortOn cost (xs ++ ys)) [start] []
\end{code}


Here's an updated `solveMaze'` driven by best-first search, which takes a cost
function expressed in terms of mazes.

\begin{code}
solveMaze' :: Ord a => (Maze -> a) -> Maze -> Maybe Maze
solveMaze' cost mz@(Maze (w,h) _ _) =  
    let entry = (1,1)
        exit = (w, h)
    in bestFirstSearch ((==exit) . head . mazePath) 
                       nextPaths
                       cost
                       (mz { mazePath = [entry]})
\end{code}


A simple cost function is one that returns the length of the path through the
maze thus far. Let's write it:

\begin{code}
bfsSolveMaze :: Maze -> Maybe Maze
bfsSolveMaze = solveMaze' (\(Maze _ path _) -> length path)
\end{code}


-- Informed Search

<Discuss> Informed search strategies

Let's implement a maze solver, again based on best-first search, that uses an
estimate of the remaining distance to the exit as a cost function:

\begin{code}
bfsSolveMaze' :: Maze -> Maybe Maze
bfsSolveMaze' mz@(Maze (w,h) _ _) = 
  solveMaze' (\(Maze _ path@((x,y):_) _) -> (w-x)+(h-y)) mz
\end{code}

The strategy above is *greedy*. How would it perform on the following maze?

    +---+---+---+---+---+---+---+---+---+---+
    |Entry          |                       |
    +   +---+---+   +   +---+---+---+---+   +
    |   |           |   |       |       |   |
    +   +   +---+---+   +   +   +   +   +   +
    |   |               |   |   |   |   |   |
    +   +---+---+---+---+   +   +   +   +   +
    |   |       |       |   |   |   |   |   |
    +   +   +   +   +   +   +   +   +   +   +
    |   |   |   |   |   |   |   |   |   |   |
    +   +   +   +   +   +   +   +   +   +   +
    |   |   |   |   |   |   |   |   |   |   |
    +   +   +   +   +   +   +   +   +   +   +
    |       |       |       |       |   Exit|
    +---+---+---+---+---+---+---+---+---+---+

\begin{code}
testMaze2 :: Maze
testMaze2 = build locs $ Maze (10,7) [] empty
  where build [l1] mz = mz
        build (l1:l2:ls) mz = build (l2:ls) $ openWall l1 l2 mz
        locs = [(1,1),(2,1),(3,1),(4,1),(4,2),(3,2),(2,2),(2,3),
                (3,3),(4,3),(5,3),(5,2),(5,1),(6,1),(7,1),(8,1),
                (9,1),(10,1),(10,2),(10,3),(10,4),(10,5),(10,6),
                (10,7),(9,7),(9,6),(9,5),(9,4),(9,3),(9,2),(8,2),
                (8,3),(8,4),(8,5),(8,6),(8,7),(7,7),(7,6),(7,5),
                (7,4),(7,3),(7,2),(6,2),(6,3),(6,4),(6,5),(6,6),
                (6,7),(5,7),(5,6),(5,5),(5,4),(4,4),(4,5),(4,6),
                (4,7),(3,7),(3,6),(3,5),(3,4),(2,4),(2,5),(2,6),
                (2,7),(1,7),(1,6),(1,5),(1,4),(1,3),(1,2),(1,1)]
\end{code}

---

A better strategy is to factor both the distance traversed so far *and* the
estimated remaining distance into the cost function. We call a best-first search
using such a cost function A* (read "A-star") search.

When our estimate of the remaining distance never overshoots the actual cost of
reaching the goal, A* search is guaranteed to return the lowest-cost path. We
call such an estimation function an *admissible heuristic*.

Let's implement an A* search solver for our maze:

\begin{code}
aStarSolveMaze :: Maze -> Maybe Maze
aStarSolveMaze mz@(Maze (w,h) _ _) = 
  solveMaze' (\(Maze _ path@((x,y):_) _) -> (w-x)+(h-y) + length path) mz
\end{code}

\begin{code}
type SPPiece = Char
type SPIndex = (Int,Int)

data SPuzzle = SP { 
    dim :: Int,
    pieces :: Array SPIndex SPPiece
  } deriving (Eq)

instance Show SPuzzle where
  show (SP n ps) = ("\n" ++)
                   $ intercalate "\n" 
                   $ map (intersperse ' ') 
                   $ chunksOf n 
                   $ Data.Array.elems ps

emptyPuzzle :: Int -> SPuzzle
emptyPuzzle n = SP n $ listArray ((1,1),(n,n)) 
                     $ (take (n^2-1) ['A'..]) ++ " " 

-- need to do it this way because simply generating a random puzzle doesn't 
-- mean it's solvable
shufflePuzzle :: SPuzzle -> StdGen -> SPuzzle
shufflePuzzle puz@(SP n ps) gen = foldl rand puz 
                                        (take (n^3) $ randoms gen :: [Int])
  where rand puz r = let alts = spuzMoves puz
                     in alts !! (r `mod` (length alts))

spuzLoc :: SPuzzle -> SPPiece -> (Int,Int)
spuzLoc (SP n ps) p = let (Just i) = findIndex (==p) $ Data.Array.elems ps
                      in ((i `div` n)+1, (i `mod` n)+1)


spuzMoves :: SPuzzle -> [SPuzzle]
spuzMoves puz@(SP n ps) = 
  let (r,c) = spuzLoc puz ' '
      adj = [(r+dr,c+dc) | dr <- [-1,0,1], dc <- [-1,0,1], 
                           (dr==0 && dc/=0) || (dr/=0 && dc==0),
                           inN (r+dr) && inN (c+dc)]
      inN i = i >= 1 && i <= n
  in [SP n (ps // [((r,c), ps!(r',c')), ((r',c'), ' ')]) | (r',c') <- adj] 

spuzScore :: SPuzzle -> Int
spuzScore puz@(SP n ps) = sum 
                          $ zipWith dist (map (spuzLoc puz) 
                                              (take (n^2-1) ['A'..]))
                          $ [(r,c) | r <- [1..n], c <- [1..n]]
  where dist (r1,c1) (r2,c2) = abs (r1-r2) + abs (c1-c2) 

data SPSol = SPSol { solPath :: [SPuzzle] }

instance Show SPSol where
  show (SPSol ps) = show $ head ps

instance Eq SPSol where
  (SPSol (p1:_)) == (SPSol (p2:_)) = p1 == p2

solvePuzzle :: SPuzzle -> SPSol
solvePuzzle puz@(SP n ps) = 
  let puzDone = emptyPuzzle n
  in fromJust $ bestFirstSearch ((==puzDone) . head . solPath)
                                nextSol
                                cost
                                (SPSol [puz])
  where nextSol (SPSol ps@(p:_)) = [SPSol (np:ps) | np <- spuzMoves p, 
                                                    not (np `elem` ps)]
        cost (SPSol ps@(p:_)) = length ps + spuzScore p

replaySol :: SPSol -> IO ()
replaySol (SPSol ps) = 
  forM_ (reverse ps) $ \p -> do 
    clearScreen
    setCursorPosition 0 0
    print p
    threadDelay $ 10^6

\end{code}
