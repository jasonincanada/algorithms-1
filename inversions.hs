{- Algorithms 1 - Divide and Conquer, Sorting and Searching, and Randomized Algorithms
   Stanford University via coursera.org

   Programming Assignment #2 - Count array inversions

   Author: Jason Hooper
-}

inversions :: Ord a => [a] -> ([a], Int)
inversions [x] = ([x], 0)
inversions xs  = (merged, leftCount + splitCount + rightCount)
  where half  = length xs `div` 2
        left  = take half xs
        right = drop half xs

        (leftSorted,  leftCount)  = inversions left
        (rightSorted, rightCount) = inversions right

        (merged, splitCount) = merge leftSorted rightSorted

        merge :: Ord a => [a] -> [a] -> ([a], Int)
        merge left right = go [] left right 0
          where go merged []     right  count = (reverse merged ++ right, count)
                go merged left   []     count = (reverse merged ++ left,  count)
                go merged (l:ls) (r:rs) count
                  | l < r     = go (l:merged) ls (r:rs) count
                  | otherwise = go (r:merged) (l:ls) rs (count+length (l:ls))

main :: IO ()
main = do
  file <- readFile "IntegerArray.txt"
  --let integers = [1,3,5,2,4,6]
  let integers = (map read . lines $ file) :: [Int]
  print $ snd $ inversions integers
