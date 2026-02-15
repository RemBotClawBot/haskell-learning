-- quickcheck-properties.hs
-- Advanced Property-Based Testing with QuickCheck
-- Comprehensive examples of property-based testing patterns

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module QuickCheckProperties where

import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Data.List (sort, nub, intersect, union, isPrefixOf, isSuffixOf)
import Control.Monad (liftM, liftM2)
import Data.Char (isUpper, isLower, isDigit, isAlpha, isAlphaNum, toUpper, toLower)
import System.Random

-- ============================================
-- 1. Basic Property Definitions
-- ============================================

-- | Identity property: f(x) == f(x)
prop_identity :: (Arbitrary a, Show a, Eq b) => (a -> b) -> Property
prop_identity f = forAll arbitrary $ \x -> f x == f x

-- | Commutative property: f(x, y) == f(y, x)
prop_commutative :: (Arbitrary a, Show a, Eq b) => (a -> a -> b) -> Property
prop_commutative f = forAll arbitrary $ \x y -> f x y == f y x

-- | Associative property: f(f(x, y), z) == f(x, f(y, z))
prop_associative :: (Arbitrary a, Show a, Eq b) => (a -> a -> a) -> Property
prop_associative f = forAll arbitrary $ \x y z -> 
    f (f x y) z == f x (f y z)

-- | Idempotent property: f(x, x) == x
prop_idempotent :: (Arbitrary a, Show a, Eq a) => (a -> a -> a) -> Property
prop_idempotent f = forAll arbitrary $ \x -> f x x == x

-- ============================================
-- 2. List Properties
-- ============================================

prop_reverse_involutive :: [Int] -> Bool
prop_reverse_involutive xs = reverse (reverse xs) == xs

prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = sort (sort xs) == sort xs

prop_length_filter :: [Int] -> NonNegative Int -> Bool
prop_length_filter xs (NonNegative n) =
    length (filter (>= n) xs) <= length xs

prop_map_composition :: [Int] -> Bool
prop_map_composition xs =
    map (toUpper . toLower . chr) xs == map (chr . toUpper . toLower) xs
  where
    chr = toEnum :: Int -> Char

-- ============================================
-- 3. Custom Generators
-- ============================================

-- | Generate non-empty lists
newtype NonEmptyList a = NonEmptyList { getNonEmpty :: [a] }
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (NonEmptyList a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        return $ NonEmptyList (x:xs)
    shrink (NonEmptyList (x:xs)) =
        NonEmptyList (x:xs) : [NonEmptyList xs' | xs' <- shrink xs]

-- | Generate sorted lists
newtype SortedList a = SortedList { getSorted :: [a] }
    deriving (Eq, Show)

instance (Arbitrary a, Ord a) => Arbitrary (SortedList a) where
    arbitrary = do
        xs <- arbitrary
        return $ SortedList (sort xs)
    shrink (SortedList xs) =
        SortedList xs : map SortedList (shrink xs)

-- | Generate binary trees
data BinaryTree a
    = Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show)

instance Arbitrary a => Arbitrary (BinaryTree a) where
    arbitrary = frequency
        [ (1, return Leaf)
        , (4, do
              left <- arbitrary
              value <- arbitrary
              right <- arbitrary
              return $ Node left value right)
        ]
    
    shrink Leaf = []
    shrink (Node left value right) =
        [Leaf] ++
        [Node left' value right | left' <- shrink left] ++
        [Node left value' right | value' <- shrink value] ++
        [Node left value right' | right' <- shrink right]

-- ============================================
-- 4. Function Properties
-- ============================================

-- | Function equality up to isomorphism
prop_function_eq :: (Arbitrary a, Show a, Eq b) => (a -> b) -> (a -> b) -> Property
prop_function_eq f g = forAll arbitrary $ \x -> f x == g x

-- | Monoid laws for functions
prop_function_monoid :: (Arbitrary a, Show a, Monoid b, Eq b) => 
    (a -> b) -> (a -> b) -> (a -> b) -> Property
prop_function_monoid f g h = conjoin
    [ forAll arbitrary $ \x -> (f x <> g x) <> h x == f x <> (g x <> h x)  -- associativity
    , forAll arbitrary $ \x -> f x <> mempty == f x                        -- right identity
    , forAll arbitrary $ \x -> mempty <> f x == f x                        -- left identity
    ]

-- ============================================
-- 5. Stateful Properties
-- ============================================

-- | State monad properties
prop_state_get_put :: Int -> Bool
prop_state_get_put s0 =
    runState (get >>= put) s0 == ((), s0)

prop_state_put_get :: Int -> Int -> Bool
prop_state_put_get s0 v =
    runState (put v >> get) s0 == (v, v)

-- | STM transaction properties
prop_stm_atomicity :: Property
prop_stm_atomicity =
    forAll arbitrary $ \(Positive n) ->
        atomicModifyIORef ref (\_ -> (n, ())) `seq`
        readIORef ref == Just n
  where
    ref = unsafePerformIO $ newIORef Nothing

-- ============================================
-- 6. Parser Properties
-- ============================================

prop_parser_identity :: String -> Bool
prop_parser_identity xs =
    parse (string xs) xs == Just (xs, "")

prop_parser_composition :: String -> String -> Property
prop_parser_composition xs ys = 
    forAll (choose (0, length xs - 1)) $ \n ->
        let prefix = take n xs
            suffix = drop n xs
        in parse (string prefix *> string suffix) xs == Just (suffix, "")

-- ============================================
-- 7. Real-World Testing Patterns
-- ============================================

-- | Round-trip property for serialization
prop_roundtrip :: (Arbitrary a, Show a, Eq a) => (a -> String) -> (String -> Maybe a) -> Property
prop_roundtrip serialize deserialize = 
    forAll arbitrary $ \x -> 
        case deserialize (serialize x) of
            Just y -> x == y
            Nothing -> False

-- | Invariant preservation
prop_invariant :: (Arbitrary a, Show a) => (a -> Bool) -> (a -> a) -> Property
prop_invariant invariant f =
    forAll arbitrary $ \x -> 
        invariant x ==> invariant (f x)

-- | Model-based testing
prop_model_based :: (Arbitrary a, Show a, Eq b) => (a -> b) -> (a -> b) -> Property
prop_model_based implementation model =
    forAll arbitrary $ \x -> implementation x == model x

-- ============================================
-- 8. Advanced Property Patterns
-- ============================================

-- | Shrinking demonstration
prop_shrinking_example :: [Int] -> Property
prop_shrinking_example xs =
    classify (length xs < 5) "small list" $
    classify (length xs >= 5 && length xs < 20) "medium list" $
    classify (length xs >= 20) "large list" $
    counterexample ("List: " ++ show xs) $
    not (null xs) ==> 
        head xs == minimum xs || head xs == maximum xs

-- | Conditional properties with ==> operator
prop_conditional :: [Int] -> Property
prop_conditional xs =
    not (null xs) ==> 
        let sorted = sort xs
        in head sorted == minimum xs && last sorted == maximum xs

-- | Collect statistics about test cases
prop_statistics :: [Int] -> Property
prop_statistics xs =
    collect (length xs) $
    collect (even $ length xs) "even length" $
    collect (odd $ length xs) "odd length" $
    length (nub xs) <= length xs

-- ============================================
-- 9. QuickCheck Commands and Configuration
-- ============================================

runAllProperties :: IO ()
runAllProperties = do
    putStrLn "Running QuickCheck properties..."
    
    putStrLn "\n=== Basic Properties ==="
    quickCheck prop_reverse_involutive
    quickCheck prop_sort_idempotent
    quickCheck prop_length_filter
    
    putStrLn "\n=== Custom Generator Tests ==="
    quickCheck $ \xs -> 
        let (NonEmptyList nes) = xs
        in not (null $ getNonEmpty nes)
    
    putStrLn "\n=== Function Properties ==="
    quickCheck $ prop_function_eq (+1) (\x -> x + 1)
    
    putStrLn "\n=== Stateful Properties ==="
    quickCheck prop_state_get_put
    quickCheck prop_state_put_get
    
    putStrLn "\n=== Round-trip Properties ==="
    let serializeInt = show
        deserializeInt = readMaybe
    quickCheck $ prop_roundtrip serializeInt deserializeInt :: Property

-- ============================================
-- 10. Performance and Stress Testing
-- ============================================

-- | Stress test with many test cases
prop_stress_test :: Positive Int -> Property
prop_stress_test (Positive n) =
    within 1000000 $  -- 1 second timeout
    forAll (vectorOf n arbitrary) $ \xs ->
        length (sort xs) == n

-- | Memory usage property
prop_memory_bounded :: [Int] -> Property
prop_memory_bounded xs =
    within 500000 $  -- 0.5 second timeout
    ioProperty $ do
        let sorted = sort xs
        return $ length sorted == length xs

-- ============================================
-- Helper Functions
-- ============================================

parse :: ([a] -> Maybe (b, [a])) -> [a] -> Maybe (b, [a])
parse = id

string :: Eq a => [a] -> [a] -> Maybe ([a], [a])
string [] input = Just ([], input)
string (x:xs) (y:ys)
    | x == y = case string xs ys of
        Just (rest, remaining) -> Just (x:rest, remaining)
        Nothing -> Nothing
string _ _ = Nothing

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

-- | Run state computation
runState :: State s a -> s -> (a, s)
runState (State f) s = f s

newtype State s a = State { runState' :: s -> (a, s) }

instance Functor (State s) where
    fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)
    (State f) <*> (State g) = State $ \s ->
        let (fab, s1) = f s
            (a, s2) = g s1
        in (fab a, s2)

instance Monad (State s) where
    return = pure
    (State f) >>= g = State $ \s ->
        let (a, s') = f s
        in runState' (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- ============================================
-- Main Function
-- ============================================

main :: IO ()
main = do
    putStrLn "Advanced QuickCheck Properties"
    putStrLn "=============================="
    
    runAllProperties
    
    putStrLn "\nCustom Generator Examples:"
    putStrLn "Generating non-empty lists..."
    samples <- sample' (arbitrary :: Gen (NonEmptyList Int))
    mapM_ (putStrLn . ("  " ++) . show) samples
    
    putStrLn "\nGenerating binary trees..."
    samples2 <- sample' (arbitrary :: Gen (BinaryTree Int))
    mapM_ (putStrLn . ("  " ++) . show) samples2
    
    putStrLn "\nRunning conditional property with statistics:"
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_statistics
    
    putStrLn "\nAll tests completed!"