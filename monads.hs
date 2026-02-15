-- monads.hs
-- From intuition to real-world monadic workflows

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Monads where

import Control.Monad (guard, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT, get, put)
import Control.Monad.Trans.Writer (Writer, WriterT(..), runWriter, runWriterT, tell)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- ============================================
-- 1. Maybe monad for safe computations
-- ============================================

safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

chainedDivision :: Maybe Double
chainedDivision = do
    a <- safeDivide 100 5
    b <- safeDivide a 2
    safeDivide b 5

-- ============================================
-- 2. Either monad for explicit errors
-- ============================================

type Error = String

safeSqrt :: Double -> Either Error Double
safeSqrt x
    | x >= 0    = Right (sqrt x)
    | otherwise = Left "Cannot take sqrt of negative number"

safeLog :: Double -> Either Error Double
safeLog x
    | x > 0     = Right (log x)
    | otherwise = Left "Cannot take log of non-positive number"

pipeline :: Double -> Either Error Double
pipeline input = do
    r1 <- safeSqrt input
    r2 <- safeLog r1
    return (r2 * 3)

-- ============================================
-- 3. List monad for nondeterminism
-- ============================================

pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples limit = do
    a <- [1..limit]
    b <- [a..limit]
    c <- [b..limit]
    guard (a^2 + b^2 == c^2)
    return (a, b, c)

-- ============================================
-- 4. Writer monad for logging
-- ============================================

computationWithLog :: Writer [String] Int
computationWithLog = do
    tell ["Starting"]
    let value = 42
    tell ["Value is " ++ show value]
    return (value * 2)

-- ============================================
-- 5. State monad for explicit state threading
-- ============================================

newtype Counter a = Counter { runCounter :: StateT Int IO a }
    deriving (Functor, Applicative, Monad)

nextCount :: Counter Int
nextCount = Counter $ do
    current <- get
    let next = current + 1
    put next
    return next

countingDemo :: IO ()
countingDemo = do
    final <- evalStateT (runCounter $ sequence [nextCount, nextCount, nextCount]) 0
    putStrLn $ "Counter states: " ++ show final

-- ============================================
-- 6. Transformer example: WriterT over IO
-- ============================================

type Audit = WriterT [String] IO

createUser :: String -> Audit ()
createUser username = do
    tell ["Creating user: " ++ username]
    lift $ putStrLn ("Simulating API call for " ++ username)
    tell ["User " ++ username ++ " created"]

runAudit :: Audit () -> IO ()
runAudit action = do
    (_, logLines) <- runWriterT action
    putStrLn "Audit log:"
    mapM_ putStrLn logLines

-- ============================================
-- 7. Real-world config reader (Reader-like)
-- ============================================

data AppConfig = AppConfig
    { dbUrl :: String
    , timeoutSeconds :: Int
    } deriving (Show)

readConfig :: IO AppConfig
readConfig = do
    url <- lookupEnv "DB_URL"
    timeoutText <- lookupEnv "TIMEOUT"
    let db = fromMaybe "localhost:5432" url
        timeout = fromMaybe 30 (timeoutText >>= readMaybe)
    return (AppConfig db timeout)

-- ============================================
-- 8. Do-notation vs bind
-- ============================================

doStyle :: Maybe Int
doStyle = do
    x <- Just 10
    y <- Just 5
    return (x + y)

bindStyle :: Maybe Int
bindStyle = Just 10 >>= \x ->
    Just 5 >>= \y ->
    return (x + y)

-- ============================================
-- 9. Kleisli composition
-- ============================================

validateDigits :: String -> Maybe String
validateDigits xs
    | all isDigit xs = Just xs
    | otherwise      = Nothing

parseInt :: String -> Maybe Int
parseInt = readMaybe

positiveOnly :: Int -> Maybe Int
positiveOnly x
    | x > 0     = Just x
    | otherwise = Nothing

validateAndParse :: String -> Maybe Int
validateAndParse = validateDigits >=> parseInt >=> positiveOnly

-- ============================================
-- 10. Main demonstration
-- ============================================

main :: IO ()
main = do
    putStrLn "Monad Playground"
    putStrLn "=" ++ replicate 40 '='

    putStrLn "\n1. Maybe monad:"
    print chainedDivision

    putStrLn "\n2. Either monad:"
    print $ pipeline 16
    print $ pipeline (-1)

    putStrLn "\n3. List monad:"
    print $ take 5 (pythagoreanTriples 50)

    putStrLn "\n4. Writer monad:"
    let (value, logs) = runWriter computationWithLog
    print value
    mapM_ putStrLn logs

    putStrLn "\n5. State monad:"
    countingDemo

    putStrLn "\n6. Transformer log:"
    runAudit (createUser "rem-bot")

    putStrLn "\n7. Config reader:"
    config <- readConfig
    print config

    putStrLn "\n8. Do vs Bind:"
    print doStyle
    print bindStyle

    putStrLn "\n9. Kleisli composition:"
    print $ validateAndParse "1234"
    print $ validateAndParse "-9"

    putStrLn "\nMonad showcase complete!"