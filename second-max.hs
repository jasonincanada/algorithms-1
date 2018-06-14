{- Algorithms 1 - Divide and Conquer, Sorting and Searching, and Randomized Algorithms
   Stanford University via coursera.org

   Optional Problem #2-1: Find the second largest number in an array

   Problem: You are given as input an unsorted array of n distinct numbers, where n is a
            power of 2.  Give an algorithm that identifies the second-largest number in the
            array, and that uses at most

                n + log_2 n - 2

            comparisons.

   Remarks: This is my second attempt and is an improvement on the first attempt, though it
            still uses too many comparisons:

                theta( n + n/2 - 3    if n >= 4
                       1              if n = 2  )

            Comparisons as a function of input size:

                n   |  1st soln  2nd soln  Goal
                ------------------------------------
                2   |  1         1         1
                4   |  5         3         4
                8   |  13        9         9
                16  |  29        21        18
                32  |  61        45        35
                64  |  125       93        68
                128 |  253       189       133
                256 |  509       381       262
-}

data Node a = Leaf           a
            | SortedPair     a a
            | TwoSortedPairs a a a a

secondMax :: Ord a => [a] -> a
secondMax ms
  | length ms < 2 = error "Expecting 2 or more elements in input"
  | otherwise     = case fold ms of
                      SortedPair     _ n     -> n
                      TwoSortedPairs _ b c _ -> max b c

fold :: Ord a => [a] -> Node a
fold [m] = Leaf m
fold ms  = combine (fold left) (fold right)
             where left  = take half ms
                   right = drop half ms
                   half  = length ms `div` 2

combine :: Ord a => Node a -> Node a -> Node a
combine (Leaf m) (Leaf n)
  | m > n     = SortedPair m n
  | otherwise = SortedPair n m

combine (SortedPair m n) (SortedPair o p)
  | m > o     = TwoSortedPairs m n o p
  | otherwise = TwoSortedPairs o p m n

combine (TwoSortedPairs a b c d) (TwoSortedPairs w x y z)
  | a > w     = if c > w
                then TwoSortedPairs a b c d
                else TwoSortedPairs a b w x
  | otherwise = if y > a
                then TwoSortedPairs w x y z
                else TwoSortedPairs w x a b

{-
  *Main> secondMax [1..8]
  7

  *Main> secondMax [8,7..1]
  7

  *Main Data.List> nub $ map secondMax $ permutations [1..8]
  [7]

-}
