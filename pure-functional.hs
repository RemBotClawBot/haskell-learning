-- pure-functional.hs
-- Pure Functional Programming foundations in Haskell
-- Demonstrates referential transparency, immutable data flows, folds, pipelines,
-- and patterns for keeping IO at the edges of your application.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module PureFunctional where

import Data.Function ((&))
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

-- ============================================
-- 1. Referential Transparency & Substitution
-- ============================================

addTax :: Double -> Double
addTax price = price * 1.2

consistentComputation :: Bool
consistentComputation =
  let basket = [10, 20, 30]
      total v = sum (map addTax v)
  in total basket == total basket  -- Always True because addTax has no hidden effects

-- Demonstrating problems when purity is broken
impureRandom :: IO Int
impureRandom = do
  -- placeholder for randomness side effect
  pure 4  -- chosen by fair dice roll

impureComparison :: IO Bool
impureComparison = do
  a <- impureRandom
  b <- impureRandom
  pure (a == b)  -- Could be False because random picks happen twice

-- ============================================
-- 2. Immutable Data Transformations
-- ============================================

data Invoice = Invoice
  { invId :: Text
  , invSubtotal :: Int
  , invDiscount :: Double
  } deriving (Show)

netInvoice :: Invoice -> Double
netInvoice inv = fromIntegral (invSubtotal inv) * (1 - invDiscount inv)

applyDiscounts :: [Invoice] -> [Invoice]
applyDiscounts = map adjust
  where
    adjust invoice
      | invSubtotal invoice > 500 = invoice { invDiscount = 0.15 }
      | otherwise                 = invoice { invDiscount = 0.05 }

-- ============================================
-- 3. Folding & Aggregation Patterns
-- ============================================

data Sales = Sales
  { productName :: Text
  , quantity :: Int
  , revenue :: Double
  } deriving (Show)

-- Pure fold that aggregates by product
aggregateSales :: [Sales] -> Map Text (Int, Double)
aggregateSales = foldl' step M.empty
  where
    step acc Sales{..} =
      let (qty, rev) = M.findWithDefault (0, 0) productName acc
      in M.insert productName (qty + quantity, rev + revenue) acc

-- Higher-level pipeline with composition
rankedProducts :: [Sales] -> [(Text, Int, Double)]
rankedProducts =
    aggregateSales
  >>> M.toList
  >>> map (\(p, (qty, rev)) -> (p, qty, rev))
  >>> sortOn (\(_, _, rev) -> negate rev)

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

-- ============================================
-- 4. Rewriting Imperative Logic into Pure Pipelines
-- ============================================

-- Imperative-style pseudo code (for reference):
-- score = 0
-- for txn in txns:
--   if txn ok: score += txn.points
--   else score -= 1
-- return clamp(score)

data Txn = Txn
  { ok :: Bool
  , points :: Int
  } deriving (Show)

scoreTransactions :: [Txn] -> Int
scoreTransactions txns =
  txns
    & fmap delta
    & sum
    & clamp (-10) 50
  where
    delta Txn{..}
      | ok        = max 0 points
      | otherwise = -1

clamp :: Ord a => a -> a -> a -> a
clamp lo hi x = max lo (min hi x)

-- ============================================
-- 5. Keeping IO at the Boundary
-- ============================================

loadFeatureFlag :: Text -> IO Bool
loadFeatureFlag key = do
  env <- lookupEnv (T.unpack key)
  pure $ fromMaybe False (env >>= readBool)
  where
    readBool = \case
      "true"  -> Just True
      "false" -> Just False
      _       -> Nothing

featureGate :: Bool -> a -> a -> a
featureGate enabled newPath oldPath = if enabled then newPath else oldPath

runPricing :: IO Double
runPricing = do
  flag <- loadFeatureFlag "NEW_PRICING"
  let oldPrice = 100
      newPrice = addTax 80
  pure $ featureGate flag newPrice oldPrice

-- ============================================
-- 6. Composition Utilities & Pointfree Thinking
-- ============================================

-- Function composition for readability
asPipeline :: [Double] -> Double
asPipeline =
    map addTax
  >>> filter (>50)
  >>> sum
  >>> (/ 1.2)

-- Building domain specific combinators
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op transform x y = transform x `op` transform y

-- Example: compare invoices by net value
compareInvoice :: Invoice -> Invoice -> Ordering
compareInvoice = compare `on` netInvoice

-- ============================================
-- 7. Infinite Data & Lazy Evaluation
-- ============================================

-- Infinite Fibonacci stream using corecursion
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

firstNFibs :: Int -> [Integer]
firstNFibs n = take n fibs

-- Lazy unfold for running averages
runningAverage :: [Double] -> [Double]
runningAverage xs = zipWith (/) (scanl1 (+) xs) [1..]

-- ============================================
-- 8. Exercises
-- ============================================

exercises :: IO ()
exercises = do
  putStrLn "\n=== Pure Functional Programming Exercises ==="
  putStrLn "1. Rewrite an imperative temperature converter into a pure pipeline"
  putStrLn "2. Implement a referential transparency checker by substitution"
  putStrLn "3. Build an immutable ledger using folds and Maps"
  putStrLn "4. Derive a lazy infinite stream of prime numbers"
  putStrLn "5. Show how to separate IO boundary for logging without polluting core logic"

-- ============================================
-- 9. Demo Entry Point
-- ============================================

main :: IO ()
main = do
  putStrLn "=== Pure Functional Programming Showcase ==="

  putStrLn "1. Referential transparency example:"
  print consistentComputation

  putStrLn "2. Immutable invoice adjustment:"
  print $ applyDiscounts [Invoice "INV-1" 600 0, Invoice "INV-2" 200 0]

  putStrLn "3. Aggregated sales:"
  print $ rankedProducts
    [ Sales "Coffee" 10 450
    , Sales "Coffee" 8 360
    , Sales "Tea" 6 180
    ]

  putStrLn "4. Transaction scoring:"
  print $ scoreTransactions [Txn True 5, Txn False 0, Txn True 3]

  putStrLn "5. Lazy Fibonacci:"
  print $ firstNFibs 10

  exercises
