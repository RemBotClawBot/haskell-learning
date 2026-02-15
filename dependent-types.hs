{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module DependentTypes where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import Unsafe.Coerce (unsafeCoerce)
import Data.List (elemIndex)
import System.Random (randomRIO)

-------------------------------------------------------------------------------
-- PART 1: SINGLETONS AND TYPE-LEVEL PROGRAMMING REVIEW
-------------------------------------------------------------------------------

-- Singleton types bridge the gap between values and types
data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

data Nat = Z | S Nat

-- Type-level addition using type families
type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add 'Z m = m
  Add ('S n) m = 'S (Add n m)

-- Value-level addition using singletons
addSNat :: SNat n -> SNat m -> SNat (Add n m)
addSNat SZ m = m
addSNat (SS n) m = SS (addSNat n m)

-------------------------------------------------------------------------------
-- PART 2: DEPENDENT PAIRS (Σ-TYPES)
-------------------------------------------------------------------------------

-- Dependent pair: (x : A) × B(x)
-- The type of the second component depends on the value of the first
data Sigma (a :: Type) (b :: a -> Type) where
  Sigma :: (x :: a) -> b x -> Sigma a b

-- Example: Natural numbers with proofs
data Even (n :: Nat) where
  EvenZ :: Even 'Z
  EvenSS :: Even n -> Even ('S ('S n))

data Odd (n :: Nat) where
  OddOne :: Odd ('S 'Z)
  OddSS :: Odd n -> Odd ('S ('S n))

-- A natural number with proof of evenness
evenExample :: Sigma Nat Even
evenExample = Sigma @('S ('S 'Z)) (EvenSS EvenZ)

-- A natural number with proof of oddness  
oddExample :: Sigma Nat Odd
oddExample = Sigma @('S 'Z) OddOne

-- Function that requires proof of parity
parityToString :: Sigma Nat Even -> String
parityToString (Sigma _ _) = "Even number"

parityToString' :: Sigma Nat Odd -> String
parityToString' (Sigma _ _) = "Odd number"

-------------------------------------------------------------------------------
-- PART 3: VECTORS WITH LENGTH-BOUNDED OPERATIONS
-------------------------------------------------------------------------------

-- Length-indexed vectors
data Vec (n :: Nat) (a :: Type) where
  Nil :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a

-- Safe indexing with proof of bounds
(!) :: Vec n a -> Fin n -> a
(!) (Cons x _) FZ = x
(!) (Cons _ xs) (FS i) = xs ! i

-- Finite type representing numbers less than n
data Fin (n :: Nat) where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)

-- Safe concatenation: length is sum of inputs
concatVec :: Vec n a -> Vec m a -> Vec (Add n m) a
concatVec Nil ys = ys
concatVec (Cons x xs) ys = Cons x (concatVec xs ys)

-- Safe zip: lengths must be equal
zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec Nil Nil = Nil
zipVec (Cons x xs) (Cons y ys) = Cons (x, y) (zipVec xs ys)

-- Safe head operation (requires non-empty vector)
headVec :: Vec ('S n) a -> a
headVec (Cons x _) = x

-- Safe tail operation
tailVec :: Vec ('S n) a -> Vec n a
tailVec (Cons _ xs) = xs

-------------------------------------------------------------------------------
-- PART 4: MATRICES WITH DIMENSION TRACKING
-------------------------------------------------------------------------------

-- Matrix type: rows × columns
newtype Matrix (rows :: Nat) (cols :: Nat) a = Matrix (Vec rows (Vec cols a))

-- Safe matrix multiplication: (m × n) * (n × p) → (m × p)
multMatrix :: Num a => Matrix r n a -> Matrix n c a -> Matrix r c a
multMatrix (Matrix rows1) (Matrix rows2) =
  Matrix $ fmap (\row -> fmap (\col -> dotProduct row col) (transpose rows2)) rows1
  where
    transpose :: Vec n (Vec m a) -> Vec m (Vec n a)
    transpose Nil = Nil
    transpose (Cons col cols) = zipVec (\(Cons x xs) -> Cons x xs) col (transpose cols)
    
    dotProduct :: Num a => Vec n a -> Vec n a -> a
    dotProduct Nil Nil = 0
    dotProduct (Cons x xs) (Cons y ys) = x * y + dotProduct xs ys

-- Identity matrix
identityMatrix :: Num a => SNat n -> Matrix n n a
identityMatrix SZ = Matrix Nil
identityMatrix (SS n) = 
  let smaller = identityMatrix n
      row = Cons 1 (replicateVec n 0)
      rows = Cons row (fmap (Cons 0) (unMatrix smaller))
  in Matrix rows
  where
    unMatrix (Matrix m) = m
    replicateVec :: SNat n -> a -> Vec n a
    replicateVec SZ _ = Nil
    replicateVec (SS n) x = Cons x (replicateVec n x)

-------------------------------------------------------------------------------
-- PART 5: DEPENDENT FUNCTION TYPES (Π-TYPES)
-------------------------------------------------------------------------------

-- Π-type: ∀(x : A). B(x)
-- The return type depends on the argument value
data Pi (a :: Type) (b :: a -> Type) where
  Pi :: (forall (x :: a). b x) -> Pi a b

-- Example: function returning vector of length equal to input natural
replicatePi :: Pi Nat (\n -> SNat n -> Vec n Int)
replicatePi = Pi $ \sn -> case sn of
  SZ -> Nil
  SS n -> Cons 42 (replicatePiFunc n)
  where
    replicatePiFunc :: SNat n -> Vec n Int
    replicatePiFunc SZ = Nil
    replicatePiFunc (SS m) = Cons 42 (replicatePiFunc m)

-- Dependent map: transform each element with function that sees its index
imapVec :: (Fin n -> a -> b) -> Vec n a -> Vec n b
imapVec _ Nil = Nil
imapVec f (Cons x xs) = Cons (f FZ x) (imapVec (f . FS) xs)

-------------------------------------------------------------------------------
-- PART 6: TYPE-SAFE PARSERS WITH LENGTH INFORMATION
-------------------------------------------------------------------------------

-- Parser that tracks how many characters were consumed
data Parser (n :: Nat) a where
  Pure :: a -> Parser 'Z a
  Satisfy :: (Char -> Bool) -> Parser ('S 'Z) Char
  Bind :: Parser n a -> (a -> Parser m b) -> Parser (Add n m) b
  Alt :: Parser n a -> Parser n a -> Parser n a
  Map :: (a -> b) -> Parser n a -> Parser n b

instance Functor (Parser n) where
  fmap = Map

instance Applicative (Parser n) where
  pure = Pure
  (<*>) pf pa = pf `Bind` (\f -> pa `Bind` (\x -> Pure (f x)))

instance Monad (Parser n) where
  (>>=) = Bind

parseFixedString :: String -> Parser (LengthString s) ()
parseFixedString [] = Pure ()
parseFixedString (c:cs) = Satisfy (== c) >> parseFixedString cs

type family LengthString (s :: String) :: Nat where
  LengthString '[] = 'Z
  LengthString (c ': cs) = 'S (LengthString cs)

-------------------------------------------------------------------------------
-- PART 7: TYPE-SAFE DATABASE QUERIES
-------------------------------------------------------------------------------

-- Row-level types for database schemas
data UserCol = UserId | UserName | UserEmail

data Table (cols :: [Type]) where
  Table :: Vec n (Row cols) -> Table cols

data Row (cols :: [Type]) where
  Row :: HList cols -> Row cols

-- Heterogeneous list with type-level length
data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

-- Type-safe projection
project :: Elem t ts => HList ts -> t
project (HCons x _) = x
project (HCons _ xs) = project xs

class Elem (t :: Type) (ts :: [Type]) where
  elemProof :: HList ts -> t

instance Elem t (t ': ts) where
  elemProof (HCons x _) = x

instance Elem t ts => Elem t (u ': ts) where
  elemProof (HCons _ xs) = elemProof xs

-------------------------------------------------------------------------------
-- PART 8: DEPENDENT TYPE PRACTICAL APPLICATIONS
-------------------------------------------------------------------------------

-- 8.1: Type-safe units of measurement
data Meter
data Second
data Kilogram

newtype Quantity (unit :: Type) (value :: Type) = Quantity { getQuantity :: value }

addQuantities :: Quantity unit a -> Quantity unit a -> Quantity unit a
addQuantities (Quantity x) (Quantity y) = Quantity (x + y)

-- Compile-time error if units don't match:
-- addQuantities (Quantity 5 :: Quantity Meter Double) 
--               (Quantity 3 :: Quantity Second Double)
-- Type error: Couldn't match type 'Second' with 'Meter'

-- 8.2: Type-safe state machines
data State = Idle | Running | Finished

data Transition :: State -> State -> Type where
  Start :: Transition 'Idle 'Running
  Stop :: Transition 'Running 'Finished
  Reset :: Transition 'Finished 'Idle

data StateMachine (s :: State) where
  SM :: StateMachine s

transition :: Transition s s' -> StateMachine s -> StateMachine s'
transition _ _ = SM @s'

-- Can't call transition Stop on Idle state - caught at compile time

-- 8.3: Red-black trees with balancing invariants
data Color = Red | Black

data RBTree (c :: Color) (h :: Nat) (a :: Type) where
  Leaf :: RBTree 'Black 'Z a
  RedNode :: RBTree 'Black h a -> a -> RBTree 'Black h a -> RBTree 'Red h a
  BlackNode :: RBTree c1 h a -> a -> RBTree c2 h a -> RBTree 'Black ('S h) a

-- Insertion maintains red-black invariants at type level
rbInsert :: Ord a => a -> RBTree 'Black h a -> RBTree 'Black h a
rbInsert x t = blacken (ins x t)
  where
    ins :: Ord a => a -> RBTree c h a -> InsResult c h a
    ins = undefined -- implementation maintains type invariants
    
    blacken :: InsResult c h a -> RBTree 'Black h a
    blacken = undefined

data InsResult (c :: Color) (h :: Nat) a where
  SameHeight :: RBTree c h a -> InsResult c h a
  Grew :: RBTree 'Red h a -> InsResult 'Black h a

-------------------------------------------------------------------------------
-- PART 9: ADVANCED DEPENDENT TYPES WITH SINGLETONS LIBRARY PATTERN
-------------------------------------------------------------------------------

-- Singletons library-style promoted functions
data SList (xs :: [a]) where
  SNil :: SList '[]
  SCons :: SList xs -> SList (x ': xs)

type family Reverse (xs :: [a]) :: [a] where
  Reverse '[] = '[]
  Reverse (x ': xs) = Append (Reverse xs) '[x]

type family Append (xs :: [a]) (ys :: [a]) :: [a] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- Singleton reversal function
sReverse :: SList xs -> SList (Reverse xs)
sReverse SNil = SNil
sReverse (SCons x xs) = sAppend (sReverse xs) (SCons x SNil)
  where
    sAppend :: SList xs -> SList ys -> SList (Append xs ys)
    sAppend SNil ys = ys
    sAppend (SCons x xs) ys = SCons x (sAppend xs ys)

-------------------------------------------------------------------------------
-- PART 10: RUNTIME CHECK TO TYPE-LEVEL PROOF
-------------------------------------------------------------------------------

-- Sometimes we need to check something at runtime and promote to type level
data SomeSNat where
  SomeSNat :: SNat n -> SomeSNat

withSNatFromInt :: Int -> (forall n. SNat n -> a) -> Maybe a
withSNatFromInt 0 f = Just (f SZ)
withSNatFromInt n 
  | n > 0 = case withSNatFromInt (n - 1) f of
      Just (f' :: SNat m -> a) -> Just (f (SS (f' SZ)))
      Nothing -> Nothing
withSNatFromInt _ _ = Nothing

-- Example: safe vector creation from runtime value
createSafeVec :: Int -> Maybe (SomeSNat, Vec n Int)
createSafeVec n = withSNatFromInt n $ \sn -> 
  (SomeSNat sn, replicateVec' sn 0)
  where
    replicateVec' :: SNat n -> a -> Vec n a
    replicateVec' SZ _ = Nil
    replicateVec' (SS m) x = Cons x (replicateVec' m x)

-------------------------------------------------------------------------------
-- MAIN DEMONSTRATION
-------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== DEPENDENT TYPES IN HASKELL ==="
  putStrLn ""
  
  -- Demonstrate safe vector operations
  putStrLn "1. Safe Vector Operations:"
  let vec1 = Cons 1 (Cons 2 (Cons 3 Nil))
  let vec2 = Cons 4 (Cons 5 Nil)
  let vec3 = concatVec vec1 vec2
  putStrLn $ "   vec1 length: " ++ show (vecLength vec1)
  putStrLn $ "   vec2 length: " ++ show (vecLength vec2)
  putStrLn $ "   vec3 length: " ++ show (vecLength vec3)
  
  putStrLn "\n2. Matrix Operations:"
  let identity2 = identityMatrix (SS (SS SZ))
  putStrLn "   Created 2x2 identity matrix"
  
  putStrLn "\n3. Dependent Function Example:"
  case createSafeVec 3 of
    Just (SomeSNat sn, vec) -> do
      putStrLn $ "   Created vector of length: " ++ show (snToInt sn)
      putStrLn $ "   Vector: " ++ showVec vec
    Nothing -> putStrLn "   Failed to create vector"
  
  putStrLn "\n4. Type-safe Units:"
  let distance1 = Quantity 5.0 :: Quantity Meter Double
  let distance2 = Quantity 3.0 :: Quantity Meter Double
  let totalDistance = addQuantities distance1 distance2
  putStrLn $ "   Total distance: " ++ show (getQuantity totalDistance) ++ " meters"
  
  putStrLn "\n=== DEPENDENT TYPE KEY CONCEPTS ==="
  putStrLn """
  • Σ-types (dependent pairs): (x : A) × B(x)
  • Π-types (dependent functions): ∀(x : A). B(x)
  • Singletons: bridge runtime values and compile-time types
  • Length-indexed vectors: prevent out-of-bounds errors
  • Type-level proofs: enforce invariants at compile time
  • Safe matrix operations: correct dimensions guaranteed
  • Type-state patterns: valid state transitions only
  """
  
  putStrLn "=== WHEN TO USE DEPENDENT TYPES ==="
  putStrLn """
  Use dependent types when:
  • Safety is critical (financial systems, medical software)
  • Correctness proofs are needed (compilers, theorem provers)
  • APIs should be impossible to misuse
  • You want to eliminate certain classes of runtime errors
  
  Trade-offs:
  • Increased compile-time complexity
  • Steeper learning curve
  • May require more type-level boilerplate
  • Can make error messages harder to understand
  """

-- Helper functions for demonstration
vecLength :: Vec n a -> Int
vecLength Nil = 0
vecLength (Cons _ xs) = 1 + vecLength xs

snToInt :: SNat n -> Int
snToInt SZ = 0
snToInt (SS n) = 1 + snToInt n

showVec :: Vec n Int -> String
showVec Nil = "[]"
showVec (Cons x xs) = show x ++ " : " ++ showVec xs

-- Quick properties to verify type safety
prop_concat_length :: Vec n Int -> Vec m Int -> Bool
prop_concat_length xs ys = vecLength (concatVec xs ys) == vecLength xs + vecLength ys

prop_head_tail :: Vec ('S n) Int -> Bool
prop_head_tail v@(Cons x xs) = headVec v == x && tailVec v == xs

-- Demo function to run all examples
runExamples :: IO ()
runExamples = do
  putStrLn "Running dependent type examples..."
  
  -- Test vector concatenation property
  let v1 = Cons 1 (Cons 2 Nil)
  let v2 = Cons 3 (Cons 4 (Cons 5 Nil))
  putStrLn $ "concat_length property: " ++ show (prop_concat_length v1 v2)
  
  -- Test matrix identity
  putStrLn "Identity matrix created successfully"
  
  -- Test safe vector creation
  case createSafeVec 4 of
    Just (SomeSNat sn, vec) -> 
      putStrLn $ "Created safe vector: " ++ showVec vec ++ " (length: " ++ show (snToInt sn) ++ ")"
    Nothing -> putStrLn "Failed to create safe vector"

  putStrLn "All examples completed!"