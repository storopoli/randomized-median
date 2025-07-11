{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main (main) where

import Lib (median, randomizedMedian, timeIt)
import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')
import Text.Printf (printf)

-- | Main function for testing and timing
main :: IO ()
main = do
    putStrLn "Median calculation benchmark"
    putStrLn "============================"

    -- Generate test data
    let size = 10_000_001
        originalData = map fromIntegral [1 .. size] :: [Double] -- Simple test case with known median
        seed = 42

        -- Shuffle the test data
        gen = mkStdGen seed
        testData = shuffle' originalData (length originalData) gen

    -- Test exact median
    putStrLn $ "Testing with " ++ show (size :: Int) ++ " shuffled elements"
    putStrLn "\nExact median calculation:"
    (exactResult, exactTime) <- timeIt $ median testData
    case exactResult of
        Nothing -> do
            putStrLn "  Result: Failed to find median"
            putStrLn "  Time: N/A"
            putStrLn "\nError percentage: N/A"
            putStrLn "Speedup factor: N/A"
        Just exactResult -> do
            printf "  Result: %.1f\n" exactResult
            printf "  Time: %.6f seconds\n" exactTime

            -- Test randomized median
            putStrLn "\nRandomized approximate median calculation:"
            (maybeResult, approxTime) <- timeIt $ randomizedMedian testData seed
            case maybeResult of
                Nothing -> do
                    putStrLn "  Result: Failed to find median"
                    putStrLn "  Time: N/A"
                    putStrLn "\nError percentage: N/A"
                    putStrLn "Speedup factor: N/A"
                Just approxResult -> do
                    printf "  Result: %.1f\n" approxResult
                    printf "  Time: %.6f seconds\n" approxTime

                    -- Calculate error
                    let exactError = abs (exactResult - approxResult) / exactResult * 100
                    printf "\nError percentage: %.4f%%\n" exactError

                    -- Calculate speedup
                    let speedup = exactTime / approxTime
                    printf "Speedup factor: %.2fx\n" speedup
