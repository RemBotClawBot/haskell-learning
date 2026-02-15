-- performance-optimization.hs
-- Haskell Performance Optimization Patterns
-- Practical techniques for writing high-performance Haskell code

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module PerformanceOptimization where

import Control.DeepSeq (force, deepseq, NFData)
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import Data.List (foldl', sort, sortBy)
import Data.Maybe (fromMaybe)
import GHC.Exts (Int(..), (+#), (-#), (*#), (==#))
import GHC.IO (unsafePerformIO)
import GHC.Real (fromIntegral)
import System.CPUTime (getCPUTime)
import System.Memory (performGC)
import System.TimeIt (timeIt, timeItT)
import Text.Printf (printf)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Mutable as VM
import Control.Concurrent (forkIO, threadDelay, newChan, readChan, writeChan)
import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Concurrent.STM (atomically, newTVar, readTVar, writeTVar)

-- ============================================
-- 1. Strictness Control
-- ============================================

-- | Lazy fold (accumulates thunks - BAD for performance)
lazySum :: [Int] -> Int
lazySum = foldl (+) 0

-- | Strict fold using seq (GOOD)
strictSum :: [Int] -> Int
strictSum = foldl' (+) 0

-- | Using bang patterns for strict evaluation
strictSumBang :: [Int] -> Int
strictSumBang = foldl' (\acc !x -> acc + x) 0

-- | Strict data type with bang patterns
data StrictPair a b = StrictPair !a !b
    deriving (Show)

-- | Strict record fields
data User = User
    { userId :: !Int
    , userName :: !String
    , userEmail :: !String
    } deriving (Show)

-- ============================================
-- 2. List vs Vector Performance
-- ============================================

-- | Naive list processing (slow for large lists)
slowDoubleList :: [Int] -> [Int]
slowDoubleList = map (*2)

-- | Vector processing (fast)
fastDoubleVector :: V.Vector Int -> V.Vector Int
fastDoubleVector = V.map (*2)

-- | Unboxed vector (even faster)
ultraFastDoubleVector :: VU.Vector Int -> VU.Vector Int
ultraFastDoubleVector = VU.map (*2)

-- ============================================
-- 3. String Performance
-- ============================================

-- | String concatenation (BAD)
slowConcat :: [String] -> String
slowConcat = foldl (++) ""

-- | Builder pattern (BETTER)
fastConcat :: [String] -> String
fastConcat = foldl' (\acc x -> acc ++ x) ""

-- | Using Text (BEST for text processing)
textConcat :: [T.Text] -> T.Text
textConcat = T.concat

-- | ByteString for binary data
bsConcat :: [BS.ByteString] -> BS.ByteString
bsConcat = BS.concat

-- ============================================
-- 4. Memoization Patterns
-- ============================================

-- | Naive Fibonacci (exponential time)
naiveFib :: Int -> Integer
naiveFib 0 = 0
naiveFib 1 = 1
naiveFib n = naiveFib (n-1) + naiveFib (n-2)

-- | Memoized Fibonacci using list
memoizedFib :: Int -> Integer
memoizedFib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Memoization with array (even faster)
fastFib :: Int -> Integer
fastFib n = fastFib' n
  where
    fastFib' 0 = 0
    fastFib' 1 = 1
    fastFib' k = memo ! (k-1) + memo ! (k-2)
    
    memo = listArray (0, n) [fastFib' i | i <- [0..n]]

-- ============================================
-- 5. Loop Fusion and Rewrite Rules
-- ============================================

-- | Multiple passes over list (BAD)
multiplePasses :: [Int] -> (Int, Int)
multiplePasses xs = (sum xs, product xs)

-- | Single pass using fold (GOOD)
singlePass :: [Int] -> (Int, Int)
singlePass = foldl' go (0, 1)
  where
    go (!accSum, !accProd) x = (accSum + x, accProd * x)

-- | Stream fusion example
{-# RULES
  "map/map"    forall f g xs. map f (map g xs) = map (f . g) xs
  "filter/map" forall p f xs. filter p (map f xs) = map f (filter (p . f) xs)
  #-}

-- ============================================
-- 6. Inlining and Specialization
-- ============================================

-- | Function marked for inlining
{-# INLINE add #-}
add :: Int -> Int -> Int
add x y = x + y

-- | Specialized version for Int
{-# SPECIALIZE sumList :: [Int] -> Int #-}
sumList :: Num a => [a] -> a
sumList = foldl' (+) 0

-- ============================================
-- 7. Unboxed Types and Primitives
-- ============================================

-- | Using unboxed primitives
unboxedSum :: Int -> Int -> Int
unboxedSum (I# x) (I# y) = I# (x +# y)

-- | Unboxed tuple return
unboxedProd :: Int -> Int -> (# Int, Int #)
unboxedProd (I# x) (I# y) = (# I# (x *# y), I# (x +# y) #)

-- ============================================
-- 8. Memory Management
-- ============================================

-- | Force evaluation to avoid space leaks
forceEvaluation :: [Int] -> IO ()
forceEvaluation xs = do
    let !result = sum xs  -- Force evaluation
    print result

-- | Deep seq for nested structures
deepForce :: (NFData a) => a -> IO ()
deepForce x = do
    evaluate (force x)
    return ()

-- ============================================
-- 9. Concurrency Patterns
-- ============================================

-- | Parallel map using forkIO
parMap :: (a -> b) -> [a] -> IO [b]
parMap f xs = do
    mvars <- mapM (\x -> do
        mvar <- newMVar undefined
        forkIO $ do
            result <- evaluate (f x)
            putMVar mvar result
        return mvar) xs
    
    mapM takeMVar mvars

-- | Worker pool pattern
workerPool :: Int -> (a -> b) -> [a] -> IO [b]
workerPool numWorkers f inputs = do
    chan <- newChan
    resultsChan <- newChan
    
    -- Create workers
    forM_ [1..numWorkers] $ \_ -> forkIO $ worker chan resultsChan
    
    -- Feed inputs
    mapM_ (writeChan chan) inputs
    replicateM (length inputs) (writeChan chan Nothing)  -- Sentinel
    
    -- Collect results
    results <- replicateM (length inputs) (readChan resultsChan)
    return results
  
  where
    worker inChan outChan = do
        task <- readChan inChan
        case task of
            Just input -> do
                let result = f input
                writeChan outChan result
                worker inChan outChan
            Nothing -> return ()  -- Stop worker

-- ============================================
-- 10. Streaming and Lazy IO
-- ============================================

-- | Lazy file reading (reads whole file into memory)
lazyReadFile :: FilePath -> IO String
lazyReadFile = readFile

-- | Streaming file processing
streamProcessFile :: FilePath -> (String -> a) -> IO [a]
streamProcessFile filePath processor = do
    contents <- readFile filePath
    return $ map processor (lines contents)

-- ============================================
-- 11. Benchmarking Utilities
-- ============================================

-- | Time a computation in seconds
timeItSeconds :: IO a -> IO (Double, a)
timeItSeconds action = do
    start <- getCPUTime
    result <- action
    end <- getCPUTime
    let cpuTime = fromIntegral (end - start) / (10 ^ 12)  -- Convert picoseconds to seconds
    return (cpuTime, result)

-- | Benchmark multiple implementations
benchmark :: String -> (Int -> a) -> Int -> IO ()
benchmark name f size = do
    putStr $ printf "%-20s: " name
    (time, _) <- timeItSeconds $ evaluate $ f size
    putStrLn $ printf "%.6f seconds (size: %d)" time size

-- | Compare performance of different implementations
comparePerformance :: Int -> IO ()
comparePerformance size = do
    putStrLn $ "\nPerformance comparison (size: " ++ show size ++ ")"
    putStrLn "=" ++ replicate 50 '='
    
    let list = [1..size]
    let vector = V.fromList list
    let uvector = VU.fromList list
    
    putStrLn "\nSum implementations:"
    benchmark "Lazy sum (list)" lazySum size
    benchmark "Strict sum (list)" strictSum size
    benchmark "Vector sum" (V.sum . V.fromList . take size) size
    benchmark "Unboxed vector sum" (VU.sum . VU.fromList . take size) size
    
    putStrLn "\nMap (*2) implementations:"
    benchmark "List map" (sum . map (*2) . take size) size
    benchmark "Vector map" (V.sum . V.map (*2) . V.fromList . take size) size
    benchmark "Unboxed vector map" (VU.sum . VU.map (*2) . VU.fromList . take size) size
    
    performGC  -- Force garbage collection between tests

-- ============================================
-- 12. Cache Optimization
-- ============================================

-- | Data structure alignment for cache efficiency
data CacheFriendly a = CacheFriendly
    { cfValue1 :: !a
    , cfValue2 :: !a
    , cfValue3 :: !a
    , cfValue4 :: !a
    } deriving (Show)

-- | Array-of-structs (AoS) - less cache friendly
type AoS = [(Int, Double, String)]

-- | Struct-of-arrays (SoA) - more cache friendly
data SoA = SoA
    { ints :: VU.Vector Int
    , doubles :: VU.Vector Double
    , strings :: V.Vector String
    } deriving (Show)

-- ============================================
-- 13. Real-World Optimization Examples
-- ============================================

-- | Optimized prime sieve
primes :: Int -> [Int]
primes n = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- | More efficient prime sieve using wheel
primesOptimized :: Int -> [Int]
primesOptimized n
    | n < 2 = []
    | otherwise = 2 : 3 : sieve (tail wheel) (tail wheel) n
  where
    wheel = [6,4,2,4,2,4,6,2] ++ wheel
    
    sieve (x:xs) (d:ds) m
        | m < p2     = []
        | otherwise  = p : sieve (xs `minus` map (p*) (multipliers p)) ds m
      where
        p = x + 1
        p2 = p * p
        
    minus xs@(x:xt) ys@(y:yt)
        | x < y     = x : minus xt ys
        | x > y     = minus xs yt
        | otherwise = minus xt yt
    
    multipliers p = takeWhile (<= n `div` p) 
        [p, p + d | d <- drop 1 wheel]

-- ============================================
-- 14. GHC Optimization Flags Usage
-- ============================================

-- | Functions that benefit from specific optimizations

-- Benefits from -O2: Loop optimization
loopOptimized :: Int -> Int
loopOptimized n = go 0 n
  where
    go acc 0 = acc
    go acc k = go (acc + k) (k - 1)

-- Benefits from -funbox-strict-fields
data UnboxedFields = UnboxedFields
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !Double
    {-# UNPACK #-} !Int
    deriving (Show)

-- ============================================
-- 15. Memory Profiling Patterns
-- ============================================

-- | Function that retains memory (creates space leak)
spaceLeak :: Int -> Int
spaceLeak n = sum . map (+1) $ [1..n]

-- | Fixed version using foldl'
spaceLeakFixed :: Int -> Int
spaceLeakFixed n = foldl' (\acc x -> acc + x + 1) 0 [1..n]

-- ============================================
-- 16. Common Performance Pitfalls and Solutions
-- ============================================

-- | Pitfall: Repeated list traversal
pitfallRepeatedTraversal :: [Int] -> (Int, Int, Int)
pitfallRepeatedTraversal xs = (sum xs, product xs, length xs)  -- BAD: 3 traversals

-- | Solution: Single traversal
solutionSingleTraversal :: [Int] -> (Int, Int, Int)
solutionSingleTraversal = foldl' go (0, 1, 0)
  where
    go (!s, !p, !l) x = (s + x, p * x, l + 1)

-- | Pitfall: Lazy pattern matching in recursive functions
pitfallLazyPattern :: [Int] -> Int
pitfallLazyPattern [] = 0
pitfallLazyPattern (x:xs) = x + pitfallLazyPattern xs  -- Thunk accumulation

-- | Solution: Strict pattern matching
solutionStrictPattern :: [Int] -> Int
solutionStrictPattern [] = 0
solutionStrictPattern (x:xs) = x `seq` (x + solutionStrictPattern xs)

-- ============================================
-- Main Function with Performance Tests
-- ============================================

main :: IO ()
main = do
    putStrLn "Haskell Performance Optimization Patterns"
    putStrLn "========================================"
    
    let testSize = 1000000
    
    putStrLn "\n=== Strictness Control ==="
    print $ strictSum [1..100]
    print $ strictSumBang [1..100]
    
    putStrLn "\n=== List vs Vector Performance ==="
    let listData = [1..testSize `div` 10]
    let vectorData = V.fromList listData
    let uvectorData = VU.fromList listData
    
    putStrLn $ "List length: " ++ show (length listData)
    putStrLn $ "Vector length: " ++ show (V.length vectorData)
    
    putStrLn "\n=== Memoization Example ==="
    putStrLn $ "Fibonacci(30) naive: " ++ show (naiveFib 30)
    putStrLn $ "Fibonacci(30) memoized: " ++ show (memoizedFib 30)
    
    putStrLn "\n=== Performance Comparison ==="
    comparePerformance 1000000
    
    putStrLn "\n=== Common Pitfalls ==="
    let xs = [1..1000]
    putStrLn $ "Repeated traversal: " ++ show (pitfallRepeatedTraversal xs)
    putStrLn $ "Single traversal: " ++ show (solutionSingleTraversal xs)
    
    putStrLn "\n=== Cache Optimization ==="
    let soa = SoA 
            (VU.fromList [1..10])
            (VU.fromList [1.0, 2.0..10.0])
            (V.fromList $ map show [1..10])
    print soa
    
    putStrLn "\nPerformance optimization examples completed!"