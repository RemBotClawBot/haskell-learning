-- typesystem.hs
-- Exploring Haskell's Type System: ADTs, type classes, GADTs, and type-level techniques

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module TypeSystem where

import Data.Kind (Type)
import GHC.TypeLits (Nat, type (+))

-- ============================================
-- 1. Algebraic Data Types (ADTs)
-- ============================================

data MyBool = MyTrue | MyFalse deriving (Show, Eq)

data Color = Red | Green | Blue | Yellow deriving (Show, Eq)

-- Parameterized ADT

data Result e a = Failure e | Success a deriving (Show, Eq)

-- Recursive ADT
data BinaryTree a
    = Empty
    | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show, Eq)

insertTree :: Ord a => a -> BinaryTree a -> BinaryTree a
insertTree x Empty = Node x Empty Empty
insertTree x (Node y left right)
    | x < y     = Node y (insertTree x left) right
    | otherwise = Node y left (insertTree x right)

-- ============================================
-- 2. Type Classes and Instances
-- ============================================

class Describable a where
    describe :: a -> String

instance Describable Color where
    describe Red    = "Energetic red"
    describe Green  = "Calming green"
    describe Blue   = "Reliable blue"
    describe Yellow = "Optimistic yellow"

instance (Describable a) => Describable [a] where
    describe xs = "List of " ++ show (length xs) ++ " items"

class Convertible a b where
    convert :: a -> b

instance Convertible Int String where
    convert = show

instance Convertible String Int where
    convert = read

-- ============================================
-- 3. Higher-kinded types and Functor hierarchy
-- ============================================

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
    fmap' _ Nothing  = Nothing
    fmap' g (Just x) = Just (g x)

class Functor' f => Applicative' f where
    pure' :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b

instance Applicative' Maybe where
    pure' = Just
    Nothing <**> _ = Nothing
    Just f  <**> mx = fmap' f mx

class Applicative' m => Monad' m where
    bind' :: m a -> (a -> m b) -> m b

instance Monad' Maybe where
    bind' Nothing _  = Nothing
    bind' (Just x) f = f x

-- ============================================
-- 4. Generalized Algebraic Data Types (GADTs)
-- ============================================

data Expr a where
    LitInt  :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add     :: Expr Int -> Expr Int -> Expr Int
    Equals  :: Expr Int -> Expr Int -> Expr Bool
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a

interpret :: Expr a -> a
interpret (LitInt n) = n
interpret (LitBool b) = b
interpret (Add e1 e2) = interpret e1 + interpret e2
interpret (Equals e1 e2) = interpret e1 == interpret e2
interpret (If cond tBranch fBranch) =
    if interpret cond then interpret tBranch else interpret fBranch

-- ============================================
-- 5. Type Families and Type-level Programming
-- ============================================

type family Element t :: Type where
    Element [a]      = a
    Element (Maybe a) = a
    Element (Result e a) = a

class Container c where
    type Item c :: Type
    singleton :: Item c -> c

instance Container [a] where
    type Item [a] = a
    singleton x = [x]

instance Container (Maybe a) where
    type Item (Maybe a) = a
    singleton = Just

-- Length-indexed vectors (type-level natural numbers)
data Vector (n :: Nat) a where
    VNil  :: Vector 0 a
    VCons :: a -> Vector n a -> Vector (n + 1) a

vHead :: Vector (n + 1) a -> a
vHead (VCons x _) = x

vectorToList :: Vector n a -> [a]
vectorToList VNil = []
vectorToList (VCons x xs) = x : vectorToList xs

vectorExample :: Vector 3 Int
vectorExample = VCons 1 (VCons 2 (VCons 3 VNil))

-- ============================================
-- 6. Smart constructors and invariants
-- ============================================

newtype Email = Email String deriving (Show)

mkEmail :: String -> Maybe Email
mkEmail raw
    | '@' `elem` raw = Just (Email raw)
    | otherwise      = Nothing

-- ============================================
-- 7. Main demonstration
-- ============================================

sampleTree :: BinaryTree Int
sampleTree = foldr insertTree Empty [7,3,10,1,5]

exprExample :: Expr Bool
exprExample = If (Equals (Add (LitInt 2) (LitInt 2)) (LitInt 4))
                 (LitBool True)
                 (LitBool False)

main :: IO ()
main = do
    putStrLn "Type System Tour"
    putStrLn "=" ++ replicate 40 '='

    putStrLn "
1. Algebraic Data Types"
    print sampleTree

    putStrLn "
2. Type Classes"
    putStrLn $ describe Red
    putStrLn $ describe [Red, Blue]

    putStrLn "
3. GADT Interpreter"
    print $ interpret exprExample

    putStrLn "
4. Length-indexed Vector"
    print $ vectorToList vectorExample
    print $ vHead vectorExample

    putStrLn "
5. Smart Constructor"
    print $ mkEmail "haskell@example.com"
    print $ mkEmail "invalid"

    putStrLn "
Type system exploration complete!"
