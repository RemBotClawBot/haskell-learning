-- exercises.hs
-- Practice problems for deepening Haskell knowledge

module Exercises where

import Data.List (nub, sort)
import Control.Monad (replicateM)

-- ============================================
-- 1. Pure functional exercises
-- ============================================

-- Implement a safe head using Maybe
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Remove duplicates while preserving order
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = go []
  where
    go _ [] = []
    go seen (x:xs)
        | x `elem` seen = go seen xs
        | otherwise     = x : go (seen ++ [x]) xs

-- Balanced parentheses checker
isBalanced :: String -> Bool
isBalanced = go 0
  where
    go 0 [] = True
    go n [] = False
    go n (c:cs)
        | c == '('  = go (n + 1) cs
        | c == ')' && n > 0 = go (n - 1) cs
        | c == ')' = False
        | otherwise = go n cs

-- ============================================
-- 2. Type class exercises
-- ============================================

class Pretty a where
    pretty :: a -> String

instance Pretty Bool where
    pretty True = "Yes"
    pretty False = "No"

instance Pretty Int where
    pretty = show

instance Pretty a => Pretty [a] where
    pretty xs = "[" ++ go xs ++ "]"
      where
        go [] = ""
        go [y] = pretty y
        go (y:ys) = pretty y ++ ", " ++ go ys

-- ============================================
-- 3. Monad exercises
-- ============================================

-- Create a logging monad for debugging
newtype Log a = Log { runLog :: (a, [String]) }

instance Functor Log where
    fmap f (Log (x, logs)) = Log (f x, logs)

instance Applicative Log where
    pure x = Log (x, [])
    Log (f, logs) <*> Log (x, logs') = Log (f x, logs ++ logs')

instance Monad Log where
    return = pure
    Log (x, logs) >>= f =
        let Log (y, newLogs) = f x
        in Log (y, logs ++ newLogs)

logMsg :: String -> Log ()
logMsg msg = Log ((), [msg])

-- Example exercise: annotate computation
annotatedSum :: [Int] -> Log Int
annotatedSum xs = do
    logMsg $ "Received list: " ++ show xs
    let s = sum xs
    logMsg $ "Computed sum: " ++ show s
    return s

-- ============================================
-- 4. Lazy evaluation exercises
-- ============================================

-- Define a custom infinite stream of squares
squares :: [Integer]
squares = [n * n | n <- [1..]]

-- Fetch n-th square lazily
nthSquare :: Int -> Integer
nthSquare n = squares !! (n - 1)

-- ============================================
-- 5. Real-world style exercise
-- ============================================

data OrderStatus = Pending | Paid | Shipped | Delivered deriving (Show, Eq)

data Order = Order
    { orderId :: Int
    , status :: OrderStatus
    , total :: Double
    } deriving (Show, Eq)

updateStatus :: Order -> OrderStatus -> Order
updateStatus order newStatus = order { status = newStatus }

totalRevenue :: [Order] -> Double
totalRevenue = sum . map total . filter ((== Delivered) . status)

-- ============================================
-- Solutions entry point
-- ============================================

main :: IO ()
main = do
    putStrLn "Exercises Demo"
    putStrLn "=" ++ replicate 40 '='

    putStrLn "\n1. Safe Head:"
    print $ safeHead ([] :: [Int])
    print $ safeHead [1,2,3]

    putStrLn "\n2. Duplicate Removal:"
    print $ removeDuplicates "haskell"

    putStrLn "\n3. Balanced Parentheses:"
    print $ isBalanced "(())()"
    print $ isBalanced "(()"

    putStrLn "\n4. Pretty instances:"
    print $ pretty [True, False, True]

    putStrLn "\n5. Logging Monad:"
    let (result, logs) = runLog (annotatedSum [1..5])
    print result
    mapM_ putStrLn logs

    putStrLn "\n6. Lazy Squares:"
    print $ nthSquare 10

    putStrLn "\n7. Order Revenue:"
    let orders = [ Order 1 Delivered 120.0, Order 2 Paid 90.0, Order 3 Delivered 50.0 ]
    print $ totalRevenue orders

    putStrLn "\nExercises complete!"