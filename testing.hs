-- testing.hs
-- Testing in Haskell: QuickCheck, Hspec, Hedgehog, and Property-Based Testing
-- Demonstrates testing methodologies, property verification, and practical testing patterns

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Testing where

import Control.Exception (catch, throwIO, SomeException)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlpha, isUpper, toLower)
import Data.List (sort)
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import System.Random (Random(..), newStdGen)
import Test.QuickCheck
  ( Arbitrary(..)
  , Gen
  , Property
  , Testable
  , property
  , quickCheck
  , quickCheckWith
  , (===)
  , (.&&.)
  , (.||.)
  , (==>)
  , conjoin
  , counterexample
  , forAll
  , frequency
  , label
  , oneof
  , resize
  , suchThat
  , vectorOf
  )
import Test.QuickCheck.Arbitrary.Generic (genericArbitrary)
import qualified Test.QuickCheck.Gen as Gen
import qualified Test.QuickCheck.Property as Prop

-- ============================================
-- 1. Manual Testing Examples
-- ============================================

-- | Simple function to test
reverseString :: String -> String
reverseString = reverse

-- | Manual test function
manualTest :: IO ()
manualTest = do
  putStrLn "=== Manual Testing ==="
  
  let testCases =
        [ ("", "")
        , ("a", "a")
        , ("hello", "olleh")
        , ("racecar", "racecar")
        , ("12345", "54321")
        ]
  
  forM_ testCases $ \(input, expected) -> do
    let result = reverseString input
    if result == expected
      then putStrLn $ "✓ PASS: reverseString \"" ++ input ++ "\" = \"" ++ expected ++ "\""
      else putStrLn $ "✗ FAIL: reverseString \"" ++ input ++ "\" = \"" ++ result ++ "\" (expected \"" ++ expected ++ "\")"

-- ============================================
-- 2. QuickCheck Basics
-- ============================================

-- | Custom data type for testing
data Person = Person
  { name :: String
  , age :: Int
  , email :: Maybe String
  } deriving (Show, Eq, Generic)

instance Arbitrary Person where
  arbitrary = do
    firstName <- Gen.elements ["Alice", "Bob", "Charlie", "Diana", "Eve"]
    lastName <- Gen.elements ["Smith", "Johnson", "Williams", "Brown", "Jones"]
    ageVal <- Gen.choose (0, 120)
    hasEmail <- Gen.arbitrary
    emailVal <- if hasEmail
      then do
        user <- Gen.listOf1 (Gen.elements ['a'..'z'])
        domain <- Gen.elements ["example.com", "test.org", "demo.net"]
        return $ Just (user ++ "@" ++ domain)
      else return Nothing
    return $ Person (firstName ++ " " ++ lastName) ageVal emailVal

-- | Property: Person age should be within valid range
prop_personAgeValid :: Person -> Property
prop_personAgeValid person =
  counterexample ("Invalid age: " ++ show (age person)) $
    age person >= 0 .&&. age person <= 120

-- | Property: Name should not be empty
prop_personNameNonEmpty :: Person -> Property
prop_personNameNonEmpty person =
  counterexample ("Empty name for person: " ++ show person) $
    not (null (name person))

-- | Property: Email format validation (if present)
prop_personEmailValid :: Person -> Property
prop_personEmailValid person = case email person of
  Nothing -> property True
  Just addr ->
    counterexample ("Invalid email: " ++ addr) $
      '@' `elem` addr && length addr > 3

-- | Property: Reverse is its own inverse
prop_reverseInvolutive :: Eq a => [a] -> Property
prop_reverseInvolutive xs = reverse (reverse xs) === xs

-- | Property: Sorting is idempotent
prop_sortIdempotent :: Ord a => [a] -> Property
prop_sortIdempotent xs = sort (sort xs) === sort xs

-- ============================================
-- 3. Complex Generators
-- ============================================

-- | Generator for valid email addresses
validEmailGen :: Gen String
validEmailGen = do
  username <- Gen.listOf1 (Gen.elements ['a'..'z'])
  domain <- Gen.elements ["gmail.com", "yahoo.com", "outlook.com"]
  return $ username ++ "@" ++ domain

-- | Generator for names (title case)
nameGen :: Gen String
nameGen = do
  firstChar <- Gen.elements ['A'..'Z']
  rest <- Gen.listOf (Gen.elements ['a'..'z'])
  return $ firstChar : rest

-- | Custom data type with complex validation
data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  } deriving (Show, Eq)

instance Arbitrary User where
  arbitrary = do
    userName' <- nameGen
    userEmail' <- validEmailGen
    userAge' <- Gen.choose (13, 100)
    return $ User userName' userEmail' userAge'

-- | Property for User validation
prop_userValid :: User -> Property
prop_userValid user =
  conjoin
    [ counterexample "Name should start with uppercase" $
        all isUpper (take 1 (userName user))
    , counterexample "Name should contain only letters" $
        all isAlpha (userName user)
    , counterexample "Email should contain @" $
        '@' `elem` userEmail user
    , counterexample "Age should be >= 13" $
        userAge user >= 13
    ]

-- ============================================
-- 4. Property-Based Testing Patterns
-- ============================================

-- | Round-trip property (serialize/deserialize)
prop_jsonRoundTrip :: Person -> Property
prop_jsonRoundTrip person = 
  -- Simulating JSON round-trip
  let encodeDecode p = p -- In real code: decode . encode $ p
  in encodeDecode person === person

-- | Test monad laws
prop_monadLeftIdentity :: (Monad m, Eq (m b)) => a -> (a -> m b) -> Property
prop_monadLeftIdentity x f = (return x >>= f) === f x

prop_monadRightIdentity :: (Monad m, Eq (m a)) => m a -> Property
prop_monadRightIdentity m = (m >>= return) === m

prop_monadAssociativity :: (Monad m, Eq (m c)) => m a -> (a -> m b) -> (b -> m c) -> Property
prop_monadAssociativity m f g = ((m >>= f) >>= g) === (m >>= (\x -> f x >>= g))

-- | Test functor laws
prop_functorIdentity :: (Functor f, Eq (f a)) => f a -> Property
prop_functorIdentity fa = fmap id fa === fa

prop_functorComposition :: (Functor f, Eq (f c)) => f a -> (b -> c) -> (a -> b) -> Property
prop_functorComposition fa g h = fmap (g . h) fa === (fmap g . fmap h) fa

-- ============================================
-- 5. Testing Stateful Operations
-- ============================================

-- | Simple stack data structure
data Stack a = Stack [a] deriving (Show, Eq)

emptyStack :: Stack a
emptyStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x:xs)) = Just (x, Stack xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x:_)) = Just x

-- | Generator for Stack
instance Arbitrary a => Arbitrary (Stack a) where
  arbitrary = do
    xs <- arbitrary
    return $ Stack xs

-- | Property: Push then pop returns original element
prop_pushPop :: Int -> Stack Int -> Property
prop_pushPop x stack =
  let stack' = push x stack
  in case pop stack' of
      Nothing -> property False
      Just (y, stack'') ->
        counterexample ("Popped " ++ show y ++ " but pushed " ++ show x) $
          y === x .&&. stack'' === stack

-- | Property: Push then peek returns pushed element
prop_pushPeek :: Int -> Stack Int -> Property
prop_pushPeek x stack =
  let stack' = push x stack
  in case peek stack' of
      Nothing -> property False
      Just y -> y === x

-- | Property: Pop from empty returns Nothing
prop_popEmpty :: Property
prop_popEmpty = isNothing (pop (emptyStack :: Stack Int))

-- ============================================
-- 6. Testing with Side Effects
-- ============================================

-- | Function with IO side effects
safeReadFile :: FilePath -> IO (Either String String)
safeReadFile path = do
  result <- catch (Right <$> readFile path) handler
  return result
  where
    handler :: SomeException -> IO (Either String String)
    handler e = return $ Left ("Error reading file: " ++ show e)

-- | Mock file system for testing
data MockFS = MockFS [(FilePath, String)]

-- | Run action with mock filesystem
withMockFS :: MockFS -> IO a -> IO a
withMockFS = const  -- In reality, would intercept file operations

-- ============================================
-- 7. Testing Performance Properties
-- ============================================

-- | Property: Sorting should be faster than O(n²) for large lists
--   (This is a conceptual property - real QuickCheck doesn't time)
prop_sortComplexity :: [Int] -> Property
prop_sortComplexity xs =
  let sorted = sort xs
      isSorted [] = True
      isSorted [_] = True
      isSorted (x:y:ys) = x <= y && isSorted (y:ys)
  in counterexample "List not sorted" $ isSorted sorted

-- | Property: Function should be idempotent
prop_idempotent :: ([Int] -> [Int]) -> [Int] -> Property
prop_idempotent f xs = f (f xs) === f xs

-- ============================================
-- 8. Integrated Test Suite
-- ============================================

runAllTests :: IO ()
runAllTests = do
  putStrLn "=== Running All Tests ==="
  
  manualTest
  
  putStrLn "\n=== QuickCheck Properties ==="
  
  putStrLn "\n1. Person properties:"
  quickCheck prop_personAgeValid
  quickCheck prop_personNameNonEmpty
  quickCheck prop_personEmailValid
  
  putStrLn "\n2. List properties:"
  quickCheck (prop_reverseInvolutive :: [Int] -> Property)
  quickCheck (prop_sortIdempotent :: [Int] -> Property)
  
  putStrLn "\n3. User properties:"
  quickCheck prop_userValid
  
  putStrLn "\n4. Stack properties:"
  quickCheck (prop_pushPop :: Int -> Stack Int -> Property)
  quickCheck (prop_pushPeek :: Int -> Stack Int -> Property)
  quickCheck prop_popEmpty
  
  putStrLn "\n5. Functor laws (Maybe):"
  quickCheck (prop_functorIdentity :: Maybe Int -> Property)
  quickCheck (prop_functorComposition :: Maybe Int -> (Int -> Int) -> (Int -> Int) -> Property)
  
  putStrLn "\n6. Monad laws (Maybe):"
  quickCheck (prop_monadLeftIdentity :: Int -> (Int -> Maybe Int) -> Property)
  quickCheck (prop_monadRightIdentity :: Maybe Int -> Property)
  quickCheck (prop_monadAssociativity :: Maybe Int -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Property)
  
  putStrLn "\n7. Idempotent property:"
  let removeDuplicates = map head . group . sort
  quickCheck (prop_idempotent removeDuplicates :: [Int] -> Property)
  
  putStrLn "\n✅ All tests completed!"

-- Helper function for idempotent property
group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) = (x : takeWhile (== x) xs) : group (dropWhile (== x) xs)

-- ============================================
-- 9. Advanced QuickCheck Features
-- ============================================

-- | Using 'forAll' with custom generators
prop_customEmail :: Property
prop_customEmail = forAll validEmailGen $ \email ->
  counterexample ("Invalid generated email: " ++ email) $
    '@' `elem` email && length email > 5

-- | Using 'label' to categorize test cases
prop_listLengthProperties :: [Int] -> Property
prop_listLengthProperties xs =
  let len = length xs
  in conjoin
      [ label "empty" $ len === 0 ==> null xs
      , label "singleton" $ len === 1 ==> length xs === 1
      , label "multiple" $ len > 1 ==> not (null xs)
      ]

-- | Using 'suchThat' for filtering
prop_positiveNumbers :: Property
prop_positiveNumbers = forAll (arbitrary `suchThat` (> (0 :: Int))) $ \x ->
  x > 0

-- | Using 'frequency' for weighted generation
data TrafficLight = Red | Yellow | Green deriving (Show, Eq)

instance Arbitrary TrafficLight where
  arbitrary = frequency
    [ (30, return Red)    -- Red is most common
    , (10, return Yellow) -- Yellow is less common
    , (60, return Green)  -- Green is most common
    ]

prop_trafficLightValid :: TrafficLight -> Property
prop_trafficLightValid light = property $
  case light of
    Red -> True
    Yellow -> True
    Green -> True

-- ============================================
-- 10. Property-Based Testing Patterns
-- ============================================

-- | Model-based testing
prop_stackModel :: [Int] -> Property
prop_stackModel ops = ioProperty $ do
  -- Model: simple list
  let model = foldl (\acc op -> op : acc) [] ops
  
  -- Implementation: using our Stack
  let stackOps = foldl (\s op -> push op s) emptyStack ops
      
  -- Check equivalence
  case toList stackOps of
    Nothing -> return $ property False
    Just result -> return $ reverse result === model
  where
    toList :: Stack Int -> Maybe [Int]
    toList stack = case pop stack of
      Nothing -> Just []
      Just (x, rest) -> (x:) <$> toList rest

-- | Shrinking demonstration
prop_shrinkingExample :: [Int] -> Property
prop_shrinkingExample xs =
  not (null xs) ==>  -- Precondition
  let ys = reverse xs
  in last xs === head ys

-- ============================================
-- Exercises
-- ============================================

exercises :: IO ()
exercises = do
  putStrLn "\n=== Testing Exercises ===\n"
  
  putStrLn "1. Implement Arbitrary for a binary tree:"
  putStrLn "   data Tree a = Leaf | Node a (Tree a) (Tree a)"
  
  putStrLn "\n2. Write property tests for:"
  putStrLn "   • Tree size property: size t >= 0"
  putStrLn "   • Tree balance: height difference <= 1 for balanced trees"
  
  putStrLn "\n3. Create a Gen for valid IPv4 addresses"
  
  putStrLn "\n4. Test round-trip property for JSON encoding/decoding"
  
  putStrLn "\n5. Implement property-based test for:"
  putStrLn "   • Monoid laws (associativity, identity)"
  putStrLn "   • Functor laws for a custom type"
  
  putStrLn "\n6. Write shrinking demonstration for list maximum property"
  
  putStrLn "\n7. Create model-based test for a queue data structure"

main :: IO ()
main = do
  runAllTests
  exercises
  
  putStrLn "\n=== QuickCheck Configuration Example ==="
  putStrLn "Running with custom configuration:"
  
  let config = 
        quickCheckWith $ 
          stdArgs { maxSuccess = 500
                  , maxDiscardRatio = 10
                  , maxSize = 100
                  }
  
  putStrLn "Running extensive person tests..."
  config prop_personAgeValid
  config prop_personNameNonEmpty
  
  putStrLn "\n✅ Testing module complete!"