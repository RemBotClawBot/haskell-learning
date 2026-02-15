-- functions.hs
-- Core functional programming idioms in Haskell

module Functions where

-- Pure arithmetic with explicit types
add :: Int -> Int -> Int
add x y = x + y

multiply :: Int -> Int -> Int
multiply x y = x * y

-- Function composition and pipelines
square :: Int -> Int
square = (\x -> x * x)

composeExample :: Int -> Int
composeExample = (square . (+3)) 4

-- Higher-order functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

mapExample :: [Int]
mapExample = map square [1..5]

-- Currying and partial application
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

addFive :: Int -> Int
addFive = add 5

-- Pattern matching and recursion
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Guards
classify :: Int -> String
classify x
    | x < 0     = "negative"
    | x == 0    = "zero"
    | otherwise = "positive"

-- List comprehensions
squaresUnder :: Int -> [Int]
squaresUnder limit = [n^2 | n <- [1..limit], n^2 <= limit]

-- Local bindings
cylinderVolume :: Double -> Double -> Double
cylinderVolume r h =
    let baseArea = pi * r * r
    in baseArea * h

-- Infinite lists
naturals :: [Integer]
naturals = [0..]

-- Lazy computation example
takeTenSquares :: [Integer]
takeTenSquares = take 10 [n^2 | n <- naturals]

main :: IO ()
main = do
    putStrLn "Functional Building Blocks"
    putStrLn "=" ++ replicate 40 '='

    print $ add 2 3
    print $ multiply 4 5
    print composeExample
    print $ applyTwice (+1) 10
    print mapExample
    print $ addThree 1 2 3
    print $ addFive 7
    print $ factorial 5
    print $ fibonacci 10
    print $ classify (-2)
    print $ squaresUnder 10
    print $ cylinderVolume 2 5
    print takeTenSquares