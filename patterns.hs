-- patterns.hs
-- Common Haskell design and idiomatic patterns

module Patterns where

import Data.Char (toLower)
import Data.Foldable (foldl')

-- ============================================
-- 1. Function composition pipeline
-- ============================================

cleanInput :: String -> String
cleanInput = trim . map toLower . filter allowed
  where
    allowed c = c `elem` ['a'..'z'] || c == ' '
    trim = unwords . words

-- ============================================
-- 2. Builder pattern via record updates
-- ============================================

data Config = Config
    { host :: String
    , port :: Int
    , useTLS :: Bool
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config "localhost" 8080 False

enableTLS :: Config -> Config
enableTLS cfg = cfg { useTLS = True }

setPort :: Int -> Config -> Config
setPort p cfg = cfg { port = p }

-- ============================================
-- 3. Interpreter pattern using type classes
-- ============================================

class Interpreter lang where
    interpret :: lang -> String

data Command
    = Say String
    | Repeat Int Command
    | Sequence [Command]
    deriving (Show)

instance Interpreter Command where
    interpret (Say msg) = msg
    interpret (Repeat n cmd) = unlines (replicate n (interpret cmd))
    interpret (Sequence cmds) = unlines (map interpret cmds)

-- ============================================
-- 4. Strategy pattern with higher-order functions
-- ============================================

type Strategy = Int -> Int

doubleStrategy, squareStrategy :: Strategy
doubleStrategy = (*2)
squareStrategy x = x * x

applyStrategy :: Strategy -> [Int] -> [Int]
applyStrategy strategy = map strategy

-- ============================================
-- 5. Iterator via Foldable
-- ============================================

sumUsingFoldable :: (Foldable t, Num a) => t a -> a
sumUsingFoldable = foldl' (+) 0

-- ============================================
-- 6. Resource pattern with bracket (mini example)
-- ============================================

withResource :: IO a -> (a -> IO ()) -> (a -> IO b) -> IO b
withResource acquire release action = do
    resource <- acquire
    result <- action resource
    release resource
    return result

-- ============================================
-- 7. Main demonstration
-- ============================================

main :: IO ()
main = do
    putStrLn "Patterns Demo"
    putStrLn "=" ++ replicate 40 '='

    putStrLn "\n1. Function composition:"
    print $ cleanInput "  HASKELL 123 !!!  "

    putStrLn "\n2. Config builder:"
    print $ setPort 443 (enableTLS defaultConfig)

    putStrLn "\n3. Interpreter pattern:"
    let script = Sequence [Say "Hello", Repeat 2 (Say "World!")]
    putStrLn $ interpret script

    putStrLn "\n4. Strategies:"
    print $ applyStrategy doubleStrategy [1..5]
    print $ applyStrategy squareStrategy [1..5]

    putStrLn "\n5. Foldable sum:"
    print $ sumUsingFoldable (Just 10)
    print $ sumUsingFoldable [1..5]

    putStrLn "\nPatterns complete!"