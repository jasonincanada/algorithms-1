{- Algorithms 1 - Divide and Conquer, Sorting and Searching, and Randomized Algorithms
   Stanford University via coursera.org

   Optional Problem #2-1: Find the second largest number in an array

   Problem: You are given as input an unsorted array of n distinct numbers, where n is a
            power of 2.  Give an algorithm that identifies the second-largest number in the
            array, and that uses at most

                n + log_2 n - 2

            comparisons.

   Remarks: This is my first attempt with a naive solution that simply folds across the input,
            keeping track of the top two elements it's seen, "rotating off" smaller values as
            required.  The algorithm does find the correct answer but its performance doesn't
            meet the requirements.  In the worst case scenario (an array completely reversed
            in order), it uses

                2(n-2) + 1 = 2n - 3

            comparisons.
-}

penultimate :: Ord a => [a] -> a
penultimate (m:n:rest) = if m > n
                         then snd $ foldr f (m, n) rest
                         else snd $ foldr f (n, m) rest
  where f x (m, n) | x > m     = (x, m)
                   | x > n     = (m, x)
                   | otherwise = (m, n)

{-
  *Main> penultimate [1..8]
  7

  *Main> penultimate [8,7..1]
  7

  -- Try all permutations of [1..8] and return a list of distinct solutions
  *Main Data.List> nub $ map penultimate (permutations [1..8])
  [7]

-}
