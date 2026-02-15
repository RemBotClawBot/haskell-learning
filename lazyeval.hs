-- lazyeval.hs
-- Understanding Lazy Evaluation and Strategies in Haskell

{-# LANGUAGE BangPatterns #-}

module LazyEvaluation where

import Control.DeepSeq (deepseq, NFData)
import Control.Exception (evaluate)
import Data.List (foldl')

-- ============================================
-- 1. Lazy Evaluation Basics
-- ============================================

-- Demonstrating laziness: infinite lists
naturals :: [Integer]
naturals = [0..]

takeFirstTen :: [Integer]
takeFirstTen = take 10 naturals

-- Laziness allows defining values in terms of themselves
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

firstTwentyFibs :: [Integer]
firstTwentyFibs = take 20 fibs

-- Infinite data structures for streams
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

firstTenPrimes :: [Integer]
firstTenPrimes = take 10 primes

-- ============================================
-- 2. Thunks and Strictness
-- ============================================

-- Lazy fold (can build large thunks)
slowSum :: [Int] -> Int
slowSum = foldl (+) 0

-- Strict fold eliminates thunks
fastSum :: [Int] -> Int
fastSum = foldl' (+) 0

-- Strict application operator ($!)
strictApply :: (a -> b) -> a -> b
strictApply f x = x `seq` f x

-- seq forces evaluation to weak head normal form
exampleSeq :: Int -> Int
exampleSeq x =
    let y = x + 1
        z = y * 2
    in z `seq` y + z

-- ============================================
-- 3. Lazy Patterns and Pattern Matching
-- ============================================

-- Lazy pattern enables partial matching
pairSecond :: (a, b) -> b
pairSecond ~(_, y) = y  -- ~ makes the pattern lazy, avoids forcing the first component

-- Without lazy pattern, pattern match would force evaluation immediately

-- ============================================
-- 4. Controlling Strictness with Bang Patterns
-- ============================================

-- Using bang patterns to force evaluation
strictSum :: [Int] -> Int
strictSum = go 0
  where
    go acc [] = acc
    go acc (x:xs) =
        let acc' = acc + x
        in acc' `seq` go acc' xs

-- ============================================
-- 5. Lazy IO and Streams
-- ============================================

lazyReadFile :: FilePath -> IO String
lazyReadFile path = readFile path  -- defered until consumed

strictReadFile :: FilePath -> IO String
strictReadFile path = do
    content <- readFile path
    content `deepseq` return content  -- force evaluation immediately

-- ============================================
-- 6. Lazy Evaluation Pitfalls
-- ============================================

-- Space leak example
spaceLeak :: Int -> Int
spaceLeak n = length (filter even [1..n])

-- Forcing intermediate results to avoid leaks
spaceSafe :: Int -> Int
spaceSafe n = length $! filter even [1..n]

-- ============================================
-- 7. Lazy vs Strict Data Structures
-- ============================================

data Stream a = Cons a (Stream a)

streamHead :: Stream a -> a
streamHead (Cons x _) = x

streamTail :: Stream a -> Stream a
streamTail (Cons _ xs) = xs

streamFrom :: Integer -> Stream Integer
streamFrom n = Cons n (streamFrom (n + 1))

streamTake :: Int -> Stream a -> [a]
streamTake 0 _ = []
streamTake n (Cons x xs) = x : streamTake (n - 1) xs

-- Strict pair example
data StrictPair a b = SP !a !b deriving Show

mkStrictPair :: a -> b -> StrictPair a b
mkStrictPair x y = SP x y

-- ============================================
-- 8. Benchmarking Laziness
-- ============================================

-- Measure difference between slowSum and fastSum
compareSums :: Int -> (Int, Int)
compareSums n = (slowSum xs, fastSum xs)
  where
    xs = [1..n]

-- ============================================
-- 9. Main Demonstration
-- ============================================

main :: IO ()
main = do
    putStrLn "Lazy Evaluation in Haskell"
    putStrLn "=" ++ replicate 40 '='

    putStrLn "\n1. Infinite Structures:"
    print takeFirstTen
    print firstTwentyFibs
    print firstTenPrimes

    putStrLn "\n2. Strict vs Lazy Folds:"
    print $ compareSums 100000

    putStrLn "\n3. Lazy Pattern Example:"
    print $ pairSecond (undefined, 42)  -- Works because of lazy pattern

    putStrLn "\n4. Strict Pair Construction:"
    print $ mkStrictPair 5 10

    putStrLn "\nLazy evaluation examples completed successfully!"