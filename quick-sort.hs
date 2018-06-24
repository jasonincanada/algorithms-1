{- Algorithms 1 - Divide and Conquer, Sorting and Searching, and Randomized Algorithms
   Stanford University via coursera.org

   Programming Assignment #3 - Count QuickSort comparisons for 3 partition strategies

   Author: Jason Hooper
-}

import Stackoverflow (swap)
import Data.List     (elemIndex)

-- A partition of a list is a distinguished element, along with everything less than it (unsorted)
-- and everything greater than it (unsorted)
data Partitioned a = Partitioned [a] a [a] deriving (Show)

type Index = Int
type PivotChooser a = ([a] -> Index)

-- The three different ways of choosing a pivot, described in the problem
firstElement :: PivotChooser a
firstElement _  = 0

lastElement :: PivotChooser a
lastElement  xs = length xs - 1

midOfThree :: Ord a => PivotChooser a
midOfThree xs = let len    = length xs
                    first  = xs !! 0
                    middle = xs !! ((len-1) `div` 2)
                    last   = xs !! (len-1)
                    med    = median first middle last
                in  case elemIndex med xs of
                      Nothing -> error "Where did it go"
                      Just i  -> i
                    
-- Find the middle of three orderable values
median :: Ord a => a -> a -> a -> a
median a b c
  | largest == a = max b c
  | largest == b = max a c
  | largest == c = max a b
  where largest = max a $ max b c

partition :: Ord a => [a] -> PivotChooser a -> Partitioned a
partition xs f = Partitioned left pivot right
  where pivotAt = f xs
        swapped = swap 0 pivotAt xs
        pivot   = head swapped
        queue   = tail swapped
        (left, right) = part [] [] queue

        part l r []   = (rotate $ reverse l, reverse r)
        part l r (x:xs)
          | x < pivot = part (x:l) (rotate r) xs
          | x > pivot = part l (x:r) xs
          | otherwise = error "Problem specified that elements are distinct"

        rotate [] = []
        rotate xs = last xs : take (length xs-1) xs

quicksort :: Ord a => [a] -> PivotChooser a -> ([a], Int)
quicksort []  _ = ([],  0)
quicksort [x] _ = ([x], 0)
quicksort xs  f = let (Partitioned left pivot right) = partition xs f
                      (leftsorted,  m) = quicksort left f
                      (rightsorted, n) = quicksort right f
                      comparisons      = length xs - 1
                  in  (leftsorted ++ [pivot] ++ rightsorted, comparisons + m + n)

main :: IO ()
main = do
  file <- readFile "QuickSort.txt"
  --let integers = [1,3,5,2,4,6]
  let integers = (map read . lines $ file) :: [Int]
  print $ snd $ quicksort integers firstElement
  print $ snd $ quicksort integers lastElement
  print $ snd $ quicksort integers midOfThree

