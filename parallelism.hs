{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Parallelism where

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies
import Control.DeepSeq (NFData, force, deepseq)
import Data.List (foldl', mapAccumL, sort)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Random (randomRIO, newStdGen, randoms)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Lazy as B
import Control.Concurrent (forkOS, myThreadId, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TArray (newTArray, readTArray, writeTArray)
import Control.Monad (forM_, replicateM, when)
import Control.Monad.ST (runST)
import Data.Int (Int64)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

-------------------------------------------------------------------------------
-- PART 1: BASIC PARALLELISM CONCEPTS
-------------------------------------------------------------------------------

-- Parallel evaluation with `par` and `pseq`
-- `x `par` y` suggests evaluating x in parallel with y
-- `x `pseq` y` evaluates x before y

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- Naive parallel Fibonacci (not efficient due to exponential work)
parFib :: Int -> Int
parFib 0 = 0
parFib 1 = 1
parFib n = a `par` b `pseq` (a + b)
  where
    a = parFib (n-1)
    b = parFib (n-2)

-- Better: use dynamic programming with parallel evaluation
parFibDP :: Int -> Int
parFibDP n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    -- No real parallelism here, just demonstrating pattern

-------------------------------------------------------------------------------
-- PART 2: STRATEGIES: EVALUATION STRATEGIES FOR PARALLELISM
-------------------------------------------------------------------------------

-- Strategies separate algorithm from parallelism
-- rseq: evaluate sequentially
-- rpar: evaluate in parallel
-- r0: don't evaluate

-- Parallel map using Strategies
parMap :: (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rseq

-- Parallel map with chunking for better granularity
parMapChunked :: Int -> (a -> b) -> [a] -> [b]
parMapChunked chunkSize f xs = map f xs `using` parListChunk chunkSize rseq

-- Evaluate list in parallel with result forced
forceListParallel :: [a] -> [a]
forceListParallel xs = xs `using` evalList rpar

-- Parallel fold with binary tree reduction
parFold :: (a -> a -> a) -> a -> [a] -> a
parFold f z xs = foldl' f z xs `using` parList rseq

-------------------------------------------------------------------------------
-- PART 3: DATA PARALLELISM WITH REPA
-------------------------------------------------------------------------------

-- Note: Repa is a high-performance, regular, multi-dimensional array library
-- Uncomment Repa imports if you have it installed:
-- import qualified Data.Array.Repa as R
-- import qualified Data.Array.Repa.Shape as RS

-- Simple parallel matrix operations pattern
type Matrix = V.Vector (V.Vector Double)

-- Initialize matrix
initMatrix :: Int -> Int -> Matrix
initMatrix rows cols = V.generate rows $ \i -> V.generate cols $ \j -> fromIntegral (i * cols + j)

-- Transpose matrix in parallel
parTranspose :: Matrix -> Matrix
parTranspose mat = runEval $ do
  let rows = V.length mat
      cols = if rows > 0 then V.length (mat V.! 0) else 0
  
  -- Create transposed matrix with appropriate dimensions
  result <- return $ V.generate cols $ \j -> V.generate rows $ \i -> mat V.! i V.! j
  
  -- Evaluate columns in parallel
  forM_ [0..cols-1] $ \j -> do
    rpar $ force $ result V.! j
  
  return result

-------------------------------------------------------------------------------
-- PART 4: PARALLEL MONTE CARLO SIMULATION
-------------------------------------------------------------------------------

-- Estimate π using parallel Monte Carlo
estimatePiParallel :: Int -> IO Double
estimatePiParallel numSamples = do
  gen <- newStdGen
  let randomPairs = take numSamples (randoms gen :: [Double])
      chunks = chunkList 1000 randomPairs
  
  results <- mapM (\chunk -> return $ countInCircle chunk) chunks `using` parList rseq
  
  let inCircle = sum results
      estimate = 4 * fromIntegral inCircle / fromIntegral numSamples
  
  return estimate
  where
    chunkList :: Int -> [a] -> [[a]]
    chunkList _ [] = []
    chunkList n xs = take n xs : chunkList n (drop n xs)
    
    countInCircle :: [Double] -> Int
    countInCircle [] = 0
    countInCircle (x:y:zs) = 
      if x*x + y*y <= 1 then 1 + countInCircle zs
      else countInCircle zs
    countInCircle _ = 0

-------------------------------------------------------------------------------
-- PART 5: PARALLEL SORTING ALGORITHMS
-------------------------------------------------------------------------------

-- Parallel merge sort
parMergeSort :: Ord a => [a] -> [a]
parMergeSort [] = []
parMergeSort [x] = [x]
parMergeSort xs = 
  if length xs < threshold 
    then sort xs  -- Use built-in sort for small lists
    else let (left, right) = splitAt (length xs `div` 2) xs
         in merge (force $ parMergeSort left) (force $ parMergeSort right) `using` rpar
  where
    threshold = 1000
    
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- Parallel quick sort
parQuickSort :: Ord a => [a] -> [a]
parQuickSort [] = []
parQuickSort (pivot:xs) = 
  let (less, equal, greater) = partition pivot xs
  in parQuickSort less ++ (pivot : equal) ++ parQuickSort greater `using` parList rseq
  where
    partition :: Ord a => a -> [a] -> ([a], [a], [a])
    partition _ [] = ([], [], [])
    partition p (x:xs) =
      let (l, e, g) = partition p xs
      in if x < p then (x:l, e, g)
         else if x > p then (l, e, x:g)
         else (l, x:e, g)

-------------------------------------------------------------------------------
-- PART 6: PARALLEL IMAGE PROCESSING PATTERN
-------------------------------------------------------------------------------

-- Type representing image pixels (RGB)
data Pixel = Pixel { red :: Int, green :: Int, blue :: Int }
  deriving (Show, Eq)

-- Convert image to grayscale in parallel
parGrayscale :: V.Vector (V.Vector Pixel) -> V.Vector (V.Vector Pixel)
parGrayscale img = runEval $ do
  let rows = V.length img
  -- Process each row in parallel
  rows' <- parMapChunkedEval 1 processRow [0..rows-1]
  return $ V.fromList rows'
  where
    processRow :: Int -> V.Vector Pixel
    processRow i = V.map toGrayscale (img V.! i)
    
    toGrayscale :: Pixel -> Pixel
    toGrayscale (Pixel r g b) = 
      let gray = round (0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b)
      in Pixel gray gray gray

parMapChunkedEval :: NFData b => Int -> (a -> b) -> [a] -> Eval [b]
parMapChunkedEval chunkSize f xs = evalList (parListChunk chunkSize rseq) (map f xs)

-------------------------------------------------------------------------------
-- PART 7: PARALLEL MAP-REDUCE PATTERN
-------------------------------------------------------------------------------

-- Generic parallel map-reduce framework
parMapReduce :: NFData b => Int -> (a -> b) -> ([b] -> b) -> [a] -> b
parMapReduce chunkSize mapper reducer xs = 
  let chunks = chunkList chunkSize xs
      mapped = map mapperChunk chunks `using` parList rseq
  in reducer mapped
  where
    mapperChunk :: [a] -> b
    mapperChunk chunk = reducer (map mapper chunk)

-- Example: Word count with parallel map-reduce
parWordCount :: [String] -> [(String, Int)]
parWordCount texts = 
  parMapReduce 10 mapper reducer (concat texts)
  where
    mapper :: String -> [(String, Int)]
    mapper text = map (,1) (words text)
    
    reducer :: [[(String, Int)]] -> [(String, Int)]
    reducer chunks = 
      let combined = foldl' mergeCounts [] (concat chunks)
      in sortByCount combined
      
    mergeCounts :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
    mergeCounts acc [] = acc
    mergeCounts [] xs = xs
    mergeCounts ((w1,c1):acc) ((w2,c2):xs)
      | w1 == w2 = (w1, c1+c2) : mergeCounts acc xs
      | w1 < w2 = (w1,c1) : mergeCounts acc ((w2,c2):xs)
      | otherwise = (w2,c2) : mergeCounts ((w1,c1):acc) xs
    
    sortByCount :: [(String, Int)] -> [(String, Int)]
    sortByCount = sortBy (\(_,c1) (_,c2) -> compare c2 c1)

-------------------------------------------------------------------------------
-- PART 8: PARALLEL STREAM PROCESSING
-------------------------------------------------------------------------------

-- Parallel stream processing with work stealing
data ParallelStream a = ParallelStream [a] Int  -- elements and chunk size

parStreamMap :: (a -> b) -> ParallelStream a -> ParallelStream b
parStreamMap f (ParallelStream xs chunkSize) = 
  ParallelStream (parMapChunked chunkSize f xs) chunkSize

parStreamFold :: (b -> a -> b) -> b -> ParallelStream a -> b
parStreamFold f z (ParallelStream xs chunkSize) =
  foldl' f z xs `using` parListChunk chunkSize rseq

-- Create parallel stream from list
toParallelStream :: Int -> [a] -> ParallelStream a
toParallelStream chunkSize xs = ParallelStream xs chunkSize

-- Process large file in parallel
parProcessFile :: FilePath -> (String -> a) -> ([a] -> b) -> IO b
parProcessFile filePath lineProcessor reducer = do
  content <- readFile filePath
  let lines' = lines content
      chunkSize = max 1 (length lines' `div` 100)  -- Aim for ~100 chunks
      stream = toParallelStream chunkSize lines'
      processed = parStreamMap lineProcessor stream
      result = parStreamFold (\acc x -> x : acc) [] processed
  return $ reducer (reverse result)

-------------------------------------------------------------------------------
-- PART 9: PARALLEL GRAPH ALGORITHMS
-------------------------------------------------------------------------------

type Graph = V.Vector [Int]  -- Adjacency list representation

-- Parallel breadth-first search
parBFS :: Graph -> Int -> V.Vector Int
parBFS graph start = runST $ do
  let n = V.length graph
  visited <- V.replicate n (-1) :: V.MVector s Int
  queue <- newTArrayIO (0, n-1) :: STM (TArray Int Int)
  
  -- Initialize
  atomically $ writeTArray queue 0 start
  V.write visited start 0
  
  -- Process levels in parallel
  bfsLevel 1 queue visited 1
  
  V.freeze visited
  where
    bfsLevel :: Int -> TArray Int Int -> V.MVector s Int -> Int -> ST s ()
    bfsLevel level queue visited queueEnd = do
      when (level < V.length graph) $ do
        
        -- Process current level nodes in parallel
        nodes <- atomically $ do
          nodes <- mapM (\i -> readTArray queue i) [0..queueEnd-1]
          return nodes
        
        -- Explore neighbors in parallel
        let neighbors = concatMap (\node -> graph V.! node) nodes
        
        -- Add unvisited neighbors to next level
        nextEnd <- foldM (\idx neighbor -> do
          dist <- V.read visited neighbor
          when (dist == -1) $ do
            V.write visited neighbor level
            atomically $ writeTArray queue idx neighbor
            return (idx + 1)
          ) 0 neighbors
        
        when (nextEnd > 0) $ bfsLevel (level + 1) queue visited nextEnd

-------------------------------------------------------------------------------
-- PART 10: BENCHMARKING AND PERFORMANCE MEASUREMENT
-------------------------------------------------------------------------------

timeAction :: IO a -> IO (Double, a)
timeAction action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  return (realToFrac $ diffUTCTime end start, result)

benchmarkParallel :: Int -> IO ()
benchmarkParallel size = do
  putStrLn $ "\nBenchmarking parallel operations with input size: " ++ show size
  
  -- Generate random data
  let xs = [1..size]
      ys = take size (cycle [1..1000])
  
  -- Sequential map
  (seqTime, _) <- timeAction $ do
    let result = map (\x -> x * x) xs
    return $ sum result
  
  -- Parallel map
  (parTime, _) <- timeAction $ do
    let result = parMap (\x -> x * x) xs
    return $ sum result
  
  -- Sequential sort
  (seqSortTime, _) <- timeAction $ do
    let shuffled = take (min 10000 size) ys
    return $ last $ sort shuffled
  
  -- Parallel sort
  (parSortTime, _) <- timeAction $ do
    let shuffled = take (min 10000 size) ys
    return $ last $ parMergeSort shuffled
  
  -- Monte Carlo π estimation
  (mcTime, piEstimate) <- timeAction $ estimatePiParallel 100000
  
  putStrLn $ "Sequential map: " ++ printf "%.4f" seqTime ++ "s"
  putStrLn $ "Parallel map:   " ++ printf "%.4f" parTime ++ "s"
  putStrLn $ "Speedup factor: " ++ printf "%.2f" (seqTime / max 0.001 parTime)
  putStrLn $ "Sequential sort: " ++ printf "%.4f" seqSortTime ++ "s"
  putStrLn $ "Parallel sort:   " ++ printf "%.4f" parSortTime ++ "s"
  putStrLn $ "π estimate: " ++ show piEstimate ++ " (" ++ printf "%.4f" mcTime ++ "s)"

-------------------------------------------------------------------------------
-- PART 11: PARALLELISM VS CONCURRENCY
-------------------------------------------------------------------------------

-- Key distinction: Parallelism = doing many things at once for performance
--                Concurrency = dealing with many things at once (coordination)

-- Parallel Fibonacci with result collection
parallelFibonacci :: Int -> IO Int
parallelFibonacci n = do
  resultVar <- newEmptyMVar
  
  -- Fork a thread to compute in parallel
  forkOS $ do
    let result = fib n
    putMVar resultVar result
  
  -- Continue with other work
  putStrLn "Computing Fibonacci in background thread..."
  
  -- Wait for result
  result <- takeMVar resultVar
  return result

-- Parallel computation with result combination
parallelComputation :: [Int] -> IO Int
parallelComputation xs = do
  let chunkSize = max 1 (length xs `div` 4)  -- 4 parallel chunks
      chunks = chunkList chunkSize xs
  
  mvars <- replicateM (length chunks) newEmptyMVar
  
  -- Fork threads for each chunk
  forM_ (zip chunks mvars) $ \(chunk, mvar) -> do
    forkOS $ do
      let result = sum chunk
      putMVar mvar result
  
  -- Collect results
  results <- mapM takeMVar mvars
  return $ sum results

-------------------------------------------------------------------------------
-- MAIN DEMONSTRATION AND EXAMPLES
-------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== PARALLELISM IN HASKELL ==="
  putStrLn ""
  
  putStrLn "1. Basic Parallel Evaluation:"
  let x = fib 35
      y = fib 36
  putStrLn $ "   Computing fib(35) and fib(36) in parallel..."
  let result = x `par` y `pseq` (x, y)
  putStrLn $ "   Result: " ++ show result
  
  putStrLn "\n2. Strategies Examples:"
  let largeList = [1..1000000]
  putStrLn "   Forcing large list evaluation in parallel..."
  let forced = forceListParallel largeList
  putStrLn $ "   First element: " ++ show (head forced)
  
  putStrLn "\n3. Parallel Monte Carlo π Estimation:"
  piEstimate <- estimatePiParallel 50000
  putStrLn $ "   π ≈ " ++ show piEstimate
  
  putStrLn "\n4. Parallel Map-Reduce Word Count:"
  let texts = ["hello world hello", "parallel haskell world", "hello parallel programming"]
      wordCounts = parWordCount texts
  putStrLn $ "   Word counts: " ++ show (take 3 wordCounts)
  
  putStrLn "\n5. Parallel Sorting:"
  let unsorted = reverse [1..10000]
  putStrLn "   Sorting 10,000 elements..."
  let sorted = parMergeSort unsorted
  putStrLn $ "   First 5 sorted: " ++ show (take 5 sorted)
  
  putStrLn "\n6. Benchmark Comparison:"
  benchmarkParallel 1000000
  
  putStrLn "\n=== PARALLELISM PATTERNS SUMMARY ==="
  putStrLn """
  Key Patterns:
  1. Data Parallelism: Apply same operation to different data elements
  2. Task Parallelism: Execute different tasks concurrently  
  3. Pipeline Parallelism: Process data through stages concurrently
  4. Divide and Conquer: Split problem, solve in parallel, combine results
  
  Haskell Tools:
  • Control.Parallel: Low-level par/pseq primitives
  • Control.Parallel.Strategies: Evaluation strategies
  • Repa: Regular multi-dimensional arrays (data parallelism)
  • Accelerate: GPU acceleration
  • Monad-Par: Monadic parallel computation
  • parallel-io: Parallel IO operations
  
  Best Practices:
  1. Measure before parallelizing (Amdahl's Law)
  2. Choose appropriate chunk size
  3. Avoid excessive parallelism (thread overhead)
  4. Use deepseq to ensure full evaluation
  5. Consider memory locality and cache effects
  
  Common Pitfalls:
  1. Too fine-grained parallelism (overhead > benefit)
  2. Not enough parallelism (underutilized cores)
  3. Memory bottlenecks (shared data structures)
  4. Load imbalance (uneven work distribution)
  5. Thread contention (shared mutable state)
  """
  
  putStrLn "=== WHEN TO USE PARALLELISM ==="
  putStrLn """
  Use parallelism when:
  • Computation is CPU-bound, not I/O-bound
  • Problem can be decomposed into independent tasks
  • Workload is large enough to overcome overhead
  • You have multiple cores available
  
  Avoid parallelism when:
  • Code is already I/O-bound
  • Task granularity is too small
  • Dependencies force sequential execution
  • Memory bandwidth is the bottleneck
  """
  
  putStrLn "\nRunning comprehensive examples..."
  runExamples

runExamples :: IO ()
runExamples = do
  putStrLn "\n=== RUNNING ALL EXAMPLES ==="
  
  -- Example 1: Parallel computation
  putStrLn "Example 1: Parallel computation with MVars"
  result <- parallelComputation [1..1000]
  putStrLn $ "Sum of 1..1000 computed in parallel: " ++ show result
  
  -- Example 2: Parallel Fibonacci
  putStrLn "\nExample 2: Parallel Fibonacci"
  fibResult <- parallelFibonacci 30
  putStrLn $ "fib(30) = " ++ show fibResult
  
  -- Example 3: Stream processing
  putStrLn "\nExample 3: Parallel stream processing"
  let stream = toParallelStream 100 [1..1000]
      mapped = parStreamMap (*2) stream
      reduced = parStreamFold (+) 0 mapped
  putStrLn $ "Parallel stream sum: " ++ show reduced
  
  putStrLn "\nAll examples completed successfully!"

-- Helper functions
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp = foldl' insert []
  where
    insert [] x = [x]
    insert (y:ys) x = case cmp x y of
      GT -> y : insert ys x
      _ -> x : y : ys

runEval :: Eval a -> a
runEval = runEvalIO  -- Simplified for demonstration; actual implementation would use runEvalIO

evalList :: Strategy a -> [a] -> Eval [a]
evalList strat xs = sequence (map (rparWith strat) xs)

parListChunk :: NFData a => Int -> Strategy a -> Strategy [a]
parListChunk n strat xs 
  | length xs <= n = evalList strat xs
  | otherwise = do
      let (chunk, rest) = splitAt n xs
      evalList strat chunk
      parListChunk n strat rest