-- realworld.hs
-- Practical Haskell patterns and applications

module RealWorld where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar, withMVar)
import Control.Exception (bracket)
import Data.Time (getCurrentTime)
import Data.List (sortOn)
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.IO (IOMode(..), hPutStrLn, withFile)

-- ============================================
-- 1. Pure business logic: pricing engine
-- ============================================

data CustomerTier = Standard | Premium | Enterprise deriving (Show, Eq, Ord)

data Product = Product
    { productId :: String
    , productName :: String
    , basePrice :: Double
    , tags :: [String]
    } deriving (Show, Eq)

-- Pure pricing rules
applyPricingRules :: CustomerTier -> Product -> Double
applyPricingRules tier product =
    let base = basePrice product
        tierDiscount = case tier of
            Standard   -> 0
            Premium    -> 0.10
            Enterprise -> 0.18
        tagBoost = if "clearance" `elem` tags product then -0.30 else 0
        finalDiscount = tierDiscount + tagBoost
    in base * (1 - finalDiscount)

-- ============================================
-- 2. Functional data processing pipeline
-- ============================================

data Transaction = Transaction
    { txId :: Int
    , amount :: Double
    , category :: String
    } deriving (Show, Eq)

summarizeTransactions :: [Transaction] -> Map.Map String Double
summarizeTransactions = foldr insertTx Map.empty
  where
    insertTx tx acc = Map.insertWith (+) (category tx) (amount tx) acc

rankCategories :: Map.Map String Double -> [(String, Double)]
rankCategories = take 5 . reverse . sortOn snd . Map.toList

-- ============================================
-- 3. Monadic IO workflow: todo CLI
-- ============================================

data TodoItem = TodoItem
    { todoId :: Int
    , todoText :: String
    , completed :: Bool
    } deriving (Show, Read)

repositoryPath :: FilePath
repositoryPath = "todo.db"

loadTodos :: IO [TodoItem]
loadTodos = do
    exists <- doesFileExist repositoryPath
    if not exists
        then return []
        else do
            content <- readFile repositoryPath
            return (read content)

saveTodos :: [TodoItem] -> IO ()
saveTodos todos = writeFile repositoryPath (show todos)

addTodo :: String -> IO TodoItem
addTodo text = do
    todos <- loadTodos
    let newId = if null todos then 1 else maximum (map todoId todos) + 1
        newTodo = TodoItem newId text False
    saveTodos (newTodo : todos)
    return newTodo

completeTodo :: Int -> IO (Maybe TodoItem)
completeTodo tid = do
    todos <- loadTodos
    let (before, after) = span ((/= tid) . todoId) todos
    case after of
        [] -> return Nothing
        (item:rest) -> do
            let updated = item { completed = True }
            saveTodos (before ++ (updated : rest))
            return (Just updated)

listTodos :: IO String
listTodos = do
    todos <- loadTodos
    let format todo = concat
            [ show (todoId todo)
            , if completed todo then " [x] " else " [ ] "
            , todoText todo
            ]
    return $ unlines (map format (reverse (sortOn todoId todos)))

-- ============================================
-- 4. Concurrency example: metrics collector
-- ============================================

runMetricsCollector :: IO ()
runMetricsCollector = do
    counter <- newMVar (0 :: Int)
    _ <- forkIO $ metricsProducer counter
    metricsConsumer counter 5

metricsProducer :: MVar Int -> IO ()
metricsProducer counter = mapM_ step [1..]
  where
    step n = do
        threadDelay (500 * 1000)
        modifyMVar_ counter (\c -> return (c + n))

metricsConsumer :: MVar Int -> Int -> IO ()
metricsConsumer counter samples = go samples
  where
    go 0 = putStrLn "Metrics collection finished"
    go n = do
        value <- readMVar counter
        putStrLn $ "Current metric: " ++ show value
        threadDelay (1 * 1000000)
        go (n - 1)

-- ============================================
-- 5. Resource safety with bracket
-- ============================================

withTempLog :: FilePath -> (FilePath -> IO a) -> IO a
withTempLog path action = bracket acquire release action
  where
    acquire = do
        putStrLn $ "Opening log file: " ++ path
        return path
    release file = do
        putStrLn $ "Closing log file: " ++ file

logEvent :: FilePath -> String -> IO ()
logEvent path message = withFile path AppendMode $ \handle -> do
    timestamp <- show <$> getCurrentTime
    hPutStrLn handle (timestamp ++ " - " ++ message)

-- ============================================
-- 6. Main demo
-- ============================================

main :: IO ()
main = do
    putStrLn "Real-World Haskell Patterns"
    putStrLn "=" ++ replicate 40 '='

    putStrLn "\n1. Pricing Engine:"
    let sampleProduct = Product "p-100" "Mechanical Keyboard" 129.99 ["peripheral"]
    print $ applyPricingRules Premium sampleProduct

    putStrLn "\n2. Transaction Summary:"
    let txs = [ Transaction 1 120.5 "cloud"
              , Transaction 2 80.0 "cloud"
              , Transaction 3 200.0 "support"
              , Transaction 4 50.0 "addons"
              ]
    print $ rankCategories (summarizeTransactions txs)

    putStrLn "\n3. Todo CLI Simulation:"
    todo <- addTodo "Write Haskell docs"
    print todo
    result <- completeTodo (todoId todo)
    print result
    putStrLn =<< listTodos

    putStrLn "\n4. Metrics Collector (short run):"
    runMetricsCollector

    putStrLn "\n5. Logging Example:"
    withTempLog "app.log" $ \path -> logEvent path "Application started"

    putStrLn "\nReal-world demonstrations complete!"