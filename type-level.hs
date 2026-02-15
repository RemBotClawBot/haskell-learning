{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-Level Programming in Haskell
-- Practical examples of type-level computation, including singletons,
-- type families, and dependent types via GADTs.
module TypeLevel where

import Data.Kind (Type)
import GHC.TypeLits
import Data.Proxy

-- ============================================================
-- Part 1: Singletons - Bridging Value and Type Levels
-- ============================================================

-- A singleton type for natural numbers
data SNat :: Nat -> Type where
  SZ :: SNat 0
  SS :: SNat n -> SNat (n + 1)

deriving instance Show (SNat n)

-- Convert from value to type-level (via type class)
class KnownNat n where
  natSing :: SNat n

instance KnownNat 0 where
  natSing = SZ

instance KnownNat n => KnownNat (n + 1) where
  natSing = SS natSing

-- Get a runtime value from a type-level Nat
natVal' :: SNat n -> Integer
natVal' SZ = 0
natVal' (SS n) = 1 + natVal' n

-- Type-safe vector: length encoded in type
data Vec :: Nat -> Type -> Type where
  Nil  :: Vec 0 a
  Cons :: a -> Vec n a -> Vec (n + 1) a

deriving instance Show a => Show (Vec n a)

-- Concatenation preserves length
concatVec :: Vec m a -> Vec n a -> Vec (m + n) a
concatVec Nil ys = ys
concatVec (Cons x xs) ys = Cons x (concatVec xs ys)

-- Safe indexing
indexVec :: SNat n -> Vec (n + m) a -> a
indexVec SZ (Cons x _) = x
indexVec (SS n) (Cons _ xs) = indexVec n xs

-- ============================================================
-- Part 2: Type Families - Functions at the Type Level
-- ============================================================

-- Type family for adding type-level Nats
type family Add (x :: Nat) (y :: Nat) :: Nat where
  Add 0 y = y
  Add x y = x + y

-- Type family for reversing vectors at the type level
type family ReverseVec (n :: Nat) (m :: Nat) :: Nat where
  ReverseVec 0 m = m
  ReverseVec (n + 1) m = ReverseVec n (n + m + 1)

-- Type-level boolean operations
data Bool' = True' | False'

type family And (x :: Bool') (y :: Bool') :: Bool' where
  And True' True' = True'
  And _     _     = False'

type family Or (x :: Bool') (y :: Bool') :: Bool' where
  Or False' False' = False'
  Or _      _      = True'

type family Not (x :: Bool') :: Bool' where
  Not True'  = False'
  Not False' = True'

-- ============================================================
-- Part 3: Type-Level Lists and Operations
-- ============================================================

-- Type-level list
data List :: [Type] -> Type where
  LNil :: List '[]
  LCons :: a -> List as -> List (a ': as)

deriving instance Show a => Show (List as)

-- Type family for list length
type family Length (xs :: [Type]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

-- Type family for list concatenation
type family Concat (xs :: [Type]) (ys :: [Type]) :: [Type] where
  Concat '[] ys = ys
  Concat (x ': xs) ys = x ': Concat xs ys

-- Heterogeneous list with type-level operations
data HList :: [Type] -> Type where
  HNil :: HList '[]
  (:::) :: a -> HList as -> HList (a ': as)

infixr 5 :::

-- Safe indexing into HList using type-level Nat
class HIndex (n :: Nat) (xs :: [Type]) where
  hindex :: Proxy n -> HList xs -> IndexHList n xs

type family IndexHList (n :: Nat) (xs :: [Type]) :: Type where
  IndexHList 0 (x ': xs) = x
  IndexHList (n + 1) (x ': xs) = IndexHList n xs

instance HIndex 0 (x ': xs) where
  hindex _ (x ::: _) = x

instance HIndex n xs => HIndex (n + 1) (x ': xs) where
  hindex proxy (_ ::: xs') = hindex (Proxy @n) xs'

-- ============================================================
-- Part 4: Type-Level Programming with Constraints
-- ============================================================

-- Type-level equality constraint via type family
type family EqType (a :: k) (b :: k) :: Bool' where
  EqType a a = True'
  EqType a b = False'

-- Type-level conditional
type family If (cond :: Bool') (a :: k) (b :: k) :: k where
  If True' a b = a
  If False' a b = b

-- Type-safe function based on type-level condition
typeSafeFunction :: forall a b. (EqType a b ~ True') => a -> b -> String
typeSafeFunction _ _ = "Types are equal!"

-- ============================================================
-- Part 5: Practical Examples
-- ============================================================

-- Example 1: Type-safe matrix dimensions
data Matrix :: Nat -> Nat -> Type -> Type where
  Matrix :: Vec rows (Vec cols a) -> Matrix rows cols a

-- Transpose: swaps dimensions at type level
transpose :: Matrix rows cols a -> Matrix cols rows a
transpose (Matrix rows) = Matrix $ transpose' rows
  where
    transpose' :: Vec n (Vec m a) -> Vec m (Vec n a)
    transpose' Nil = Nil
    transpose' (Cons row rows') = zipCons row (transpose' rows')
    
    zipCons :: Vec m a -> Vec m (Vec n a) -> Vec m (Vec n a)
    zipCons Nil _ = Nil
    zipCons (Cons x xs) (Cons ys zs) = Cons (Cons x ys) (zipCons xs zs)

-- Example 2: Type-level configuration
data Config = Config
  { debugMode :: Bool'
  , logLevel :: Nat
  }

-- Using type-level config to control runtime behavior
type family ShouldLog (config :: Config) :: Bool' where
  ShouldLog ('Config True' _) = True'
  ShouldLog ('Config False' level) = If (level >? 0) True' False'

type family (>?) (x :: Nat) (y :: Nat) :: Bool' where
  x >? y = If (x <=? y) False' True'

type family (<=?) (x :: Nat) (y :: Nat) :: Bool' where
  0 <=? _ = True'
  (x + 1) <=? 0 = False'
  (x + 1) <=? (y + 1) = x <=? y

-- Example 3: Type-safe units
data Unit = Meter | Second | Kg

type family Mul (a :: Unit) (b :: Unit) :: Unit where
  Mul Meter Second = MeterSecond
  Mul Second Meter = MeterSecond
  Mul Meter Kg = MeterKg
  Mul Kg Meter = MeterKg

newtype Quantity (u :: Unit) = Quantity Double
  deriving (Show, Eq, Num)

addQuantities :: Quantity u -> Quantity u -> Quantity u
addQuantities (Quantity x) (Quantity y) = Quantity (x + y)

-- ============================================================
-- Part 6: Running Examples
-- ============================================================

main :: IO ()
main = do
  putStrLn "=== Type-Level Programming Examples ===\n"
  
  -- Singletons
  putStrLn "1. Singleton Example:"
  let three :: SNat 3
      three = SS (SS (SS SZ))
  print $ natVal' three
  
  -- Type-safe vector
  putStrLn "\n2. Type-Safe Vector:"
  let vec1 = Cons 1 (Cons 2 (Cons 3 Nil))
      vec2 = Cons 4 (Cons 5 Nil)
      vec3 = concatVec vec1 vec2
  print vec1
  print vec2
  print vec3
  
  -- HList example
  putStrLn "\n3. Heterogeneous List Indexing:"
  let hlist = 42 ::: "hello" ::: True ::: HNil
      first = hindex (Proxy @0) hlist
      second = hindex (Proxy @1) hlist
      third = hindex (Proxy @2) hlist
  putStrLn $ "First element: " ++ show first
  putStrLn $ "Second element: " ++ show second
  putStrLn $ "Third element: " ++ show third
  
  -- Matrix transpose
  putStrLn "\n4. Matrix Transpose:"
  let mat = Matrix $ Cons (Cons 1 (Cons 2 Nil)) 
                         (Cons (Cons 3 (Cons 4 Nil)) 
                         (Cons (Cons 5 (Cons 6 Nil)) Nil))
      transposed = transpose mat
  print mat
  print transposed
  
  putStrLn "\n=== Type-Level Programming Complete ==="