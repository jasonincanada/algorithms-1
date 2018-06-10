{- Algorithms 1 - Divide and Conquer, Sorting and Searching, and Randomized Algorithms
   Stanford University via coursera.org

   Programming Assignment #1 - Implement Karatsuba multiplication

   Author: Jason Hooper
-}

m, n :: Integer
m = 3141592653589793238462643383279502884197169399375105820974944592
n = 2718281828459045235360287471352662497757247093699959574966967627

karatsuba :: Integer -> Integer -> Integer
karatsuba m n
  | min m n < 10 = m * n
  | otherwise    = left + middle + right
  where shift  = ceiling $ logBase 10 (fromInteger $ min m n) / 2
        (a, b) = split m shift
        (c, d) = split n shift
        ac     = karatsuba a c
        bd     = karatsuba b d
        kara   = karatsuba (a+b) (c+d)
        left   = ac * 10^(shift*2)
        middle = (kara - ac - bd) * 10^shift
        right  = bd

split :: Integer -> Int -> (Integer, Integer)
split x shift = (left, right)
  where base  = 10 ^ shift
        left  = x `div` base
        right = x `mod` base

{-
    Main> m*n
    8539734222673567065463550869546574495034888535765114961879601127067743044893204848617875072216249073013374895871952806582723184

    Main> karatsuba m n
    8539734222673567065463550869546574495034888535765114961879601127067743044893204848617875072216249073013374895871952806582723184

    Main> m*n - karatsuba m n
    0
-}
