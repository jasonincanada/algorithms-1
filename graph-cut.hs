{- Algorithms 1 - Divide and Conquer, Sorting and Searching, and Randomized Algorithms
   Stanford University via coursera.org

   Programming Assignment #4 - Find a graph cut by removing edges at random

   Author: Jason Hooper
-}

import System.Random (randomRIO)

data Edge a  = Edge a a
data Graph a = Graph [a] [Edge a]

-- Remove an edge from a graph by merging the edge's vertices and removing any resulting loops
contract :: Eq a => Graph a -> Edge a -> Graph a
contract (Graph vs es) edge@(Edge remove keep) = Graph vertices edges
  where vertices = filter (/= remove) vs
        edges    = filter (not . isLoop) $ map (adjust remove keep) es

        isLoop (Edge v1 v2) | v1 == v2  = True
                            | otherwise = False

        adjust :: Eq a => a -> a -> Edge a -> Edge a
        adjust remove keep edge@(Edge v1 v2)
          | v1 == remove = Edge keep v2
          | v2 == remove = Edge v1 keep
          | otherwise    = edge

-- Find a cut for a graph by removing edges randomly until we have 2 vertices left
cut :: Eq a => Graph a -> IO (Graph a)
cut graph@(Graph vs es) 
  | length vs <= 2 = return graph
  | otherwise      = do idx <- randomRIO (0, length es - 1)
                        cut $ contract graph (es !! idx)

-- Turn our input file into a graph
parse :: [String] -> Graph Int
parse lines = foldr add (Graph [] []) vertices
  where vertices = map (map read . words) lines

        add :: [Int] -> Graph Int -> Graph Int
        add (source : targets) (Graph vs es) = Graph vs' es'
          where vs'   = source : vs
                es'   = edges ++ es
                edges = map (Edge source) $ filter (>source) targets

main :: IO ()
main = do
  file <- readFile "kargerMinCut.txt"
  let graph = parse $ lines file
  Graph _ edges <- cut graph
  print $ length edges

