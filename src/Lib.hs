{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lib
  ( median,
    randomizedMedian,
    timeIt,
  )
where

import Data.Array (elems, listArray, (!))
import Data.List (sort)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Random (mkStdGen, randomRs)

-- ==================== CORE MEDIAN FUNCTIONS ====================

-- | Calculates the median of a list by first sorting it.
-- Returns the middle element for odd-length lists.
-- Returns the average of two middle elements for even-length lists.
--
-- Usage:
-- >>> median [1..10000]
-- Just 5000.5
median :: (Ord a, Fractional a) => [a] -> Maybe a
median [] = Nothing
median xs =
  -- First convert list to array for O(1) random access
  let n = length xs
      arr = listArray (0, n - 1) xs
      -- Sort the array elements
      sorted = sort (elems arr)
      sortedArr = listArray (0, n - 1) sorted
      mid = n `div` 2
   in if odd n
        then Just (sortedArr ! mid)
        else Just ((sortedArr ! (mid - 1) + sortedArr ! mid) / 2)

-- | Implements the randomized median algorithm as specified.
-- This algorithm finds the exact median with high probability.
--
-- Usage:
-- >>> randomizedMedian [1..10000] 42
-- Just 5001
randomizedMedian :: (Ord a) => [a] -> Int -> Maybe a
randomizedMedian [] _ = Nothing
randomizedMedian xs seed =
  let n = length xs
      arr = listArray (0, n - 1) xs

      -- Step 1: Sample n^(3/4) elements with replacement
      sampleSize = ceiling (fromIntegral n ** (3 / 4))
      gen = mkStdGen seed
      indices = take sampleSize $ randomRs (0, n - 1) gen

      -- Step 2: Sort the sample
      sample = sort [arr ! i | i <- indices]
      sampleArr = listArray (0, length sample - 1) sample

      -- Step 3: Find d (the lower bound element)
      dIndex = floor (fromIntegral n ** (3 / 4) / 2 - sqrt (fromIntegral n))
      d =
        if dIndex >= 0 && dIndex < length sample
          then sampleArr ! dIndex
          else error "Invalid d index"

      -- Step 4: Find u (the upper bound element)
      uIndex = floor (fromIntegral n ** (3 / 4) / 2 + sqrt (fromIntegral n))
      u =
        if uIndex >= 0 && uIndex < length sample
          then sampleArr ! uIndex
          else error "Invalid u index"

      -- Step 5: Compute set C and counts
      ld = length $ filter (< d) xs
      lu = length $ filter (> u) xs
      c = sort $ filter (\x -> d <= x && x <= u) xs

      -- Step 6 & 7: Check failure conditions
      halfN = n `div` 2
   in ( if ((ld > halfN || lu > halfN) || (length c > 4 * sampleSize)) || null c
          then Nothing
          else
            ( let targetIndex = halfN - ld
               in if targetIndex >= 0 && targetIndex < length c
                    then
                      -- Step 8: Output the median
                      Just (c !! targetIndex)
                    else
                      Nothing
            )
      )

-- ==================== TESTING AND TIMING ====================

-- | Time execution of a computation.
--
-- Usage:
-- >>> timeIt $ median [1..10000]
-- (5000.5,3.37e-4)
timeIt :: a -> IO (a, Double)
timeIt x = do
  start <- getCurrentTime
  let result = x -- Force evaluation
  end <- result `seq` getCurrentTime
  let diff = realToFrac (diffUTCTime end start) :: Double
  return (result, diff)
