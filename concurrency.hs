-- concurrency.hs
-- Practical Concurrency and Parallel Programming in Haskell
-- Covers: threads, MVars, STM, async, parallelism, and real-world patterns

{-# LANGUAGE BangPatterns #-}

module Concurrency where

import Control.Concurrent (forkIO, myThreadId, threadDelay, killThread)
import Control.Concurrent.Async (Async, async, wait, race, concurrently)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_, readMVar)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar, modifyTVar', newTVarIO)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan)
import Control.Exception (catch, SomeException, bracket)
import Control.Monad (forM_, forever, replicateM_, when)
import Control.Monad.STM (orElse, retry)
import Data.Foldable (traverse_)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Data.List (sort)
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

-- ============================================
-- 1. Basic Threading with forkIO
-- ============================================

-- Simple thread creation
simpleThreads :: IO ()
simpleThreads = do
  putStrLn "Main thread starting..."
  
  -- Launch a background thread
  threadId <- forkIO $ do
    threadDelay 1000000  -- 1 second
    putStrLn "Background thread: Hello after 1 second!"
    
  putStrLn "Main thread continues immediately..."
  threadDelay 2000000  -- Wait 2 seconds
  putStrLn "Main thread done."

-- Thread communication via MVar
safeCounter :: IO ()
safeCounter = do
  counter <- newMVar (0 :: Int)
  
  -- Spawn 10 threads that increment counter
  threads <- forM_ [1..10] $ \i -> forkIO $ do
    threadDelay (i * 10000)  -- Stagger start times
    modifyMVar_ counter $ \n -> do
      putStrLn $ "Thread " ++ show i ++ " sees counter: " ++ show n
      pure (n + 1)
    
  threadDelay 500000  -- Wait for all threads
  finalCount <- readMVar counter
  putStrLn $ "Final counter value: " ++ show finalCount

-- ============================================
-- 2. Software Transactional Memory (STM)
-- ============================================

-- Bank account example with STM
type Account = TVar Double

createAccount :: Double -> IO Account
createAccount balance = newTVarIO balance

withdraw :: Account -> Double -> STM ()
withdraw account amount = do
  balance <- readTVar account
  when (balance < amount) retry  -- Block until sufficient funds
  writeTVar account (balance - amount)

deposit :: Account -> Double -> STM ()
deposit account amount = do
  balance <- readTVar account
  writeTVar account (balance + amount)

transfer :: Account -> Account -> Double -> STM ()
transfer fromAcc toAcc amount = do
  withdraw fromAcc amount
  deposit toAcc amount

-- Deadlock-free dining philosophers
philosophers :: IO ()
philosophers = do
  putStrLn "\n== Dining Philosophers with STM =="
  
  -- Create 5 forks (MVars)
  forks <- replicateM 5 (newMVar ())
  let forkIds = [0..4]
  
  -- Philosopher function
  let philosopher :: Int -> IO ()
      philosopher id = forever $ do
        putStrLn $ "Philosopher " ++ show id ++ " is thinking..."
        threadDelay =<< randomRIO (500000, 1500000)
        
        putStrLn $ "Philosopher " ++ show id ++ " is hungry..."
        
        -- Pick up forks in sorted order to prevent deadlock
        let (left, right) = if id == 4
                           then (forks !! 4, forks !! 0)  -- Last philosopher picks differently
                           else (forks !! id, forks !! (id + 1))
        
        -- Acquire both forks
        _ <- takeMVar left
        _ <- takeMVar right
        
        putStrLn $ "Philosopher " ++ show id ++ " is eating..."
        threadDelay =<< randomRIO (500000, 1500000)
        
        -- Release forks
        putMVar left ()
        putMVar right ()
    
  -- Launch philosophers
  threads <- traverse (\i -> forkIO (philosopher i)) forkIds
  
  -- Run for 10 seconds
  threadDelay 10000000
  putStrLn "Dinner time over!"
  traverse_ killThread threads

-- ============================================
-- 3. Async Programming with Control.Concurrent.Async
-- ============================================

-- Parallel computation with async
parallelComputation :: IO ()
parallelComputation = do
  putStrLn "\n== Parallel Computation =="
  
  -- Launch three expensive computations in parallel
  a1 <- async $ do
    threadDelay 1000000
    pure "Result 1"
    
  a2 <- async $ do
    threadDelay 1500000
    pure "Result 2"
    
  a3 <- async $ do
    threadDelay 800000
    pure "Result 3"
    
  -- Wait for all results
  r1 <- wait a1
  r2 <- wait a2
  r3 <- wait a3
  
  putStrLn $ "Results: " ++ show [r1, r2, r3]

-- Race conditions with `race` and `concurrently`
timeoutComputation :: IO ()
timeoutComputation = do
  putStrLn "\n== Timeout Computation =="
  
  -- This computation might take a long time
  let slowComputation = do
        threadDelay 3000000  -- 3 seconds
        pure "Slow result"
  
  -- This computation provides a timeout
  let timeout = do
        threadDelay 1000000  -- 1 second
        pure "Timeout!"
  
  -- Race between slow computation and timeout
  result <- race slowComputation timeout
  
  case result of
    Left slow -> putStrLn $ "Got slow result: " ++ slow
    Right timeoutMsg -> putStrLn timeoutMsg

-- Concurrent I/O operations
concurrentDownloads :: IO ()
concurrentDownloads = do
  putStrLn "\n== Concurrent Downloads Simulation =="
  
  let urls = ["https://api.example.com/data1"
             , "https://api.example.com/data2"
             , "https://api.example.com/data3"
             , "https://api.example.com/data4"]
  
  let download url = do
        putStrLn $ "Starting download: " ++ url
        delay <- randomRIO (500000, 2000000)
        threadDelay delay
        putStrLn $ "Completed: " ++ url
        pure (url, delay `div` 1000)
  
  -- Download all URLs concurrently
  results <- traverse (async . download) urls
  completed <- traverse wait results
  
  putStrLn "\nDownload summary:"
  forM_ completed $ \(url, ms) ->
    putStrLn $ "  " ++ url ++ " took " ++ show ms ++ "ms"

-- ============================================
-- 4. Producer-Consumer Pattern
-- ============================================

-- Message queue with STM channel
messageQueue :: IO ()
messageQueue = do
  putStrLn "\n== Producer-Consumer with TChan =="
  
  chan <- newTChanIO
  
  -- Producer: generates messages
  let producer id = forever $ do
        threadDelay =<< randomRIO (200000, 800000)
        atomically $ do
          writeTChan chan (id, "Message from producer " ++ show id)
  
  -- Consumer: processes messages
  let consumer id = forever $ do
        (prodId, msg) <- atomically $ readTChan chan
        putStrLn $ "Consumer " ++ show id ++ " received: " ++ msg ++ " from producer " ++ show prodId
        threadDelay =<< randomRIO (100000, 400000)
  
  -- Launch producers and consumers
  producers <- traverse (\i -> forkIO (producer i)) [1..3]
  consumers <- traverse (\i -> forkIO (consumer i)) [1..2]
  
  threadDelay 5000000  -- Run for 5 seconds
  traverse_ killThread (producers ++ consumers)
  putStrLn "Message queue demo complete."

-- ============================================
-- 5. Parallel Data Processing
-- ============================================

-- Parallel map using async
parMap :: (a -> IO b) -> [a] -> IO [b]
parMap f xs = do
  jobs <- traverse (async . f) xs
  traverse wait jobs

-- Parallel processing example
parallelDataProcessing :: IO ()
parallelDataProcessing = do
  putStrLn "\n== Parallel Data Processing =="
  
  let dataSet = [1..1000] :: [Int]
  
  -- Expensive computation
  let processItem n = do
        threadDelay (n `mod` 10 * 1000)  -- Simulate work
        pure $ n * n
  
  start <- getCurrentTime
  
  -- Sequential processing
  _ <- traverse processItem dataSet
  sequentialEnd <- getCurrentTime
  
  -- Parallel processing
  _ <- parMap processItem dataSet
  parallelEnd <- getCurrentTime
  
  let sequentialTime = sequentialEnd `diffUTCTime` start
      parallelTime = parallelEnd `diffUTCTime` sequentialEnd
  
  putStrLn $ "Sequential time: " ++ show sequentialTime
  putStrLn $ "Parallel time: " ++ show parallelTime
  putStrLn $ "Speedup: " ++ show (sequentialTime / parallelTime)

-- ============================================
-- 6. Resource Pool Pattern
-- ============================================

-- Database connection pool simulation
type Connection = Int
type Pool = TVar [Connection]

createPool :: Int -> IO Pool
createPool size = newTVarIO [1..size]

withConnection :: Pool -> (Connection -> IO a) -> IO a
withConnection pool action = bracket acquire release $ \conn -> action conn
  where
    acquire = atomically $ do
      conns <- readTVar pool
      case conns of
        [] -> retry  -- Wait for available connection
        (c:cs) -> do
          writeTVar pool cs
          pure c
    
    release conn = atomically $ do
      modifyTVar' pool (conn:)

connectionPoolDemo :: IO ()
connectionPoolDemo = do
  putStrLn "\n== Connection Pool Demo =="
  
  pool <- createPool 3  -- 3 connections
  
  let worker id = do
        withConnection pool $ \conn -> do
          putStrLn $ "Worker " ++ show id ++ " using connection " ++ show conn
          threadDelay =<< randomRIO (500000, 1500000)
          putStrLn $ "Worker " ++ show id ++ " releasing connection " ++ show conn
  
  workers <- traverse (\i -> forkIO (worker i)) [1..10]
  
  threadDelay 8000000  -- Run for 8 seconds
  traverse_ killThread workers
  putStrLn "Connection pool demo complete."

-- ============================================
-- 7. Real-World Pattern: Web Server Request Handler
-- ============================================

-- Simulated web request handler with rate limiting
type RequestId = Int
data Request = Request
  { reqId :: RequestId
  , reqPath :: String
  , reqBody :: String
  } deriving (Show)

data Response = Response
  { respStatus :: Int
  , respBody :: String
  } deriving (Show)

-- Rate limiter using TVar
rateLimitedHandler :: TVar Int -> Request -> IO Response
rateLimitedHandler rateLimiter req = do
  -- Check rate limit
  allowed <- atomically $ do
    current <- readTVar rateLimiter
    if current >= 10
      then pure False
      else do
        modifyTVar' rateLimiter (+1)
        pure True
  
  if not allowed
    then pure $ Response 429 "Too Many Requests"
    else do
      -- Process request
      threadDelay =<< randomRIO (100000, 500000)
      let response = Response 200 $ "Processed: " ++ reqPath req
      
      -- Decrement rate limit after delay
      threadDelay 1000000  -- 1 second window
      atomically $ modifyTVar' rateLimiter (\x -> max 0 (x - 1))
      
      pure response

-- ============================================
-- Main: Run all concurrency examples
-- ============================================

main :: IO ()
main = do
  putStrLn "=== Haskell Concurrency & Parallelism Examples ==="
  
  -- Run basic examples
  simpleThreads
  safeCounter
  
  -- Run philosophers (commented out for speed during testing)
  -- philosophers
  
  -- Async examples
  parallelComputation
  timeoutComputation
  concurrentDownloads
  
  -- Producer-consumer
  messageQueue
  
  -- Parallel processing
  parallelDataProcessing
  
  -- Resource pool
  connectionPoolDemo
  
  putStrLn "\n✅ All concurrency examples completed!"
  
  -- Rate limiting demo
  putStrLn "\n== Rate Limiting Demo =="
  rateLimiter <- newTVarIO 0
  
  let requests = [Request i ("/api/" ++ show i) ("data" ++ show i) | i <- [1..15]]
  
  responses <- traverse (rateLimitedHandler rateLimiter) requests
  
  putStrLn "Response statuses:"
  forM_ (take 10 responses) $ \resp ->
    putStrLn $ "  Status " ++ show (respStatus resp) ++ ": " ++ respBody resp