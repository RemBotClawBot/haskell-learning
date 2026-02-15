-- functional-patterns.hs
-- Advanced Functional Programming Patterns, Combinators, and Techniques
-- Covers: Lenses, Free Monads, Tagless Final, Yoneda, Cont, and practical abstractions

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module FunctionalPatterns where

import Control.Applicative (Alternative(..), Const(..))
import Control.Monad (ap, join, when, (>=>))
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (State, get, put, runState)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Char (toUpper)
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.List (intercalate)

-- ============================================
-- 1. Lenses (Van Laarhoven representation)
-- ============================================

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

-- Lens creation
lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter afb s = fmap (setter s) (afb (getter s))

-- Common lens combinators
view :: Lens s a -> s -> a
view l s = getConst (l Const s)

set :: Lens s a -> a -> s -> s
set l a s = runIdentity (l (\_ -> Identity a) s)

over :: Lens s a -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)

-- Example data type
data Person = Person
  { _name :: String
  , _age :: Int
  , _address :: Address
  } deriving (Show)

data Address = Address
  { _street :: String
  , _city :: String
  , _zipCode :: String
  } deriving (Show)

-- Lenses for Person
name :: Lens Person String
name = lens _name (\p n -> p { _name = n })

age :: Lens Person Int
age = lens _age (\p a -> p { _age = a })

address :: Lens Person Address
address = lens _address (\p addr -> p { _address = addr })

-- Lenses for Address
street :: Lens Address String
street = lens _street (\a s -> a { _street = s })

city :: Lens Address String
city = lens _city (\a c -> a { _city = c })

zipCode :: Lens Address String
zipCode = lens _zipCode (\a z -> a { _zipCode = z })

-- Composition example
personStreet :: Lens Person String
personStreet = address . street

-- ============================================
-- 2. Free Monads for DSLs
-- ============================================

data CalculatorF a
  = Add Int Int (Int -> a)
  | Subtract Int Int (Int -> a)
  | Multiply Int Int (Int -> a)
  | Divide Int Int (Int -> a)
  deriving Functor

type Calculator = Free CalculatorF

pattern AddP x y = Free (Add x y Pure)
pattern SubtractP x y = Free (Subtract x y Pure)
pattern MultiplyP x y = Free (Multiply x y Pure)
pattern DivideP x y = Free (Divide x y Pure)

-- Free monad definition
data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= k = k a
  Free fa >>= k = Free (fmap (>>= k) fa)

-- Smart constructors
add :: Int -> Int -> Calculator Int
add x y = Free (Add x y Pure)

subtract :: Int -> Int -> Calculator Int
subtract x y = Free (Subtract x y Pure)

multiply :: Int -> Int -> Calculator Int
multiply x y = Free (Multiply x y Pure)

divide :: Int -> Int -> Calculator Int
divide x y = Free (Divide x y Pure)

-- Interpreter for Calculator DSL
interpretCalculator :: Calculator a -> Either String a
interpretCalculator (Pure a) = Right a
interpretCalculator (Free (Add x y k)) = interpretCalculator (k (x + y))
interpretCalculator (Free (Subtract x y k)) = interpretCalculator (k (x - y))
interpretCalculator (Free (Multiply x y k)) = interpretCalculator (k (x * y))
interpretCalculator (Free (Divide x y k))
  | y == 0 = Left "Division by zero"
  | otherwise = interpretCalculator (k (x `div` y))

-- ============================================
-- 3. Tagless Final (Final Tagless) Encoding
-- ============================================

-- Expression language
class ExprLang repr where
  int :: Int -> repr Int
  add :: repr Int -> repr Int -> repr Int
  mul :: repr Int -> repr Int -> repr Int
  eq :: Eq a => repr a -> repr a -> repr Bool

-- Evaluation interpreter
newtype Eval a = Eval { runEval :: a }

instance ExprLang Eval where
  int = Eval
  add (Eval x) (Eval y) = Eval (x + y)
  mul (Eval x) (Eval y) = Eval (x * y)
  eq (Eval x) (Eval y) = Eval (x == y)

-- Pretty printing interpreter
newtype Pretty a = Pretty { render :: String }

instance ExprLang Pretty where
  int n = Pretty (show n)
  add (Pretty x) (Pretty y) = Pretty ("(" ++ x ++ " + " ++ y ++ ")")
  mul (Pretty x) (Pretty y) = Pretty ("(" ++ x ++ " * " ++ y ++ ")")
  eq (Pretty x) (Pretty y) = Pretty ("(" ++ x ++ " == " ++ y ++ ")")

-- Type-safe expression builder
expr1 :: ExprLang repr => repr Int
expr1 = add (int 3) (mul (int 4) (int 5))

expr2 :: ExprLang repr => repr Bool
expr2 = eq (add (int 1) (int 2)) (int 3)

-- ============================================
-- 4. Yoneda Lemma / Codensity for Optimization
-- ============================================

-- Yoneda embedding
newtype Yoneda f a = Yoneda
  { runYoneda :: forall b. (a -> b) -> f b
  }

instance Functor f => Functor (Yoneda f) where
  fmap f (Yoneda g) = Yoneda (\k -> g (k . f))

-- Embed and project
liftYoneda :: Functor f => f a -> Yoneda f a
liftYoneda fa = Yoneda (\f -> fmap f fa)

lowerYoneda :: Yoneda f a -> f a
lowerYoneda y = runYoneda y id

-- Codensity monad for optimization
newtype Codensity m a = Codensity
  { runCodensity :: forall b. (a -> m b) -> m b
  }

instance Functor (Codensity m) where
  fmap f (Codensity g) = Codensity (\k -> g (k . f))

instance Applicative (Codensity m) where
  pure x = Codensity (\k -> k x)
  Codensity f <*> Codensity g = Codensity (\k -> f (\f' -> g (k . f')))

instance Monad (Codensity m) where
  return = pure
  Codensity m >>= f = Codensity (\k -> m (\a -> runCodensity (f a) k))

-- Reassociation optimization example
reassociateList :: [Int] -> Codensity [] Int
reassociateList xs = do
  x <- Codensity (\k -> concatMap k xs)
  y <- Codensity (\k -> concatMap k xs)
  return (x + y)

-- ============================================
-- 5. Continuation Passing Style (CPS)
-- ============================================

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap f (Cont c) = Cont (\k -> c (k . f))

instance Applicative (Cont r) where
  pure x = Cont (\k -> k x)
  Cont f <*> Cont x = Cont (\k -> f (\f' -> x (\x' -> k (f' x'))))

instance Monad (Cont r) where
  return = pure
  Cont c >>= f = Cont (\k -> c (\a -> runCont (f a) k))

-- Delimited continuations
reset :: Cont a a -> a
reset (Cont c) = c id

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = Cont (\k -> runCont (f k) id)

-- CPS factorial
factorialCPS :: Int -> Cont Int Int
factorialCPS n = go n
  where
    go 0 = return 1
    go m = do
      rest <- go (m - 1)
      return (m * rest)

-- ============================================
-- 6. Comonads for Contextual Computation
-- ============================================

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

-- Non-empty list comonad
data NonEmpty a = a :| [a] deriving (Show, Functor)

instance Comonad NonEmpty where
  extract (x :| _) = x
  duplicate nel@(x :| xs) = nel :| case xs of
    [] -> []
    (y:ys) -> go y ys
   where
    go cur [] = [cur :| []]
    go cur (z:zs) = (cur :| zs) : go z zs

-- Zipper comonad
data Zipper a = Zipper [a] a [a] deriving (Show)

instance Functor Zipper where
  fmap f (Zipper left focus right) = Zipper (map f left) (f focus) (map f right)

instance Comonad Zipper where
  extract (Zipper _ focus _) = focus
  duplicate z@(Zipper left focus right) =
    Zipper (lefts left) z (rights right)
   where
    lefts [] = []
    lefts (x:xs) = Zipper xs x (focus:right) : lefts xs
    
    rights [] = []
    rights (x:xs) = Zipper (focus:left) x xs : rights xs

-- Cellular automata with comonad
cellularRule :: (Bool -> Bool -> Bool -> Bool) -> Zipper Bool -> Bool
cellularRule rule (Zipper (l:_) c (r:_)) = rule l c r
cellularRule _ _ = False

rule30 :: Bool -> Bool -> Bool -> Bool
rule30 True True True = False
rule30 True True False = True
rule30 True False True = True
rule30 True False False = True
rule30 False True True = True
rule30 False True False = True
rule30 False False True = True
rule30 False False False = False

-- ============================================
-- 7. Practical Combinators and Utilities
-- ============================================

-- Kleisli arrow composition
kleisliComposition :: [Int] -> [Int]
kleisliComposition = (filter (>0) >=> \x -> [x, x * 2])

-- Traversable utilities
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Functor)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l r) = Node <$> traverse f l <*> traverse f r

-- Natural transformations
type NatTrans f g = forall a. f a -> g a

maybeToList :: NatTrans Maybe []
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Const applicative for monoidal aggregation
monoidalSum :: [Int] -> Int
monoidalSum = getSum . foldMap (Sum . Const)

newtype Sum a = Sum { getSum :: a }
  deriving (Num, Eq, Show)

instance Num a => Semigroup (Sum a) where
  Sum x <> Sum y = Sum (x + y)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

-- ============================================
-- 8. Advanced Pattern: Validation with Applicative
-- ============================================

data Validation e a
  = Success a
  | Failure [e]
  deriving (Show, Functor)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Failure es1 <*> Failure es2 = Failure (es1 <> es2)
  Failure es <*> _ = Failure es
  _ <*> Failure es = Failure es

-- Validation combinators
validateNonEmpty :: String -> String -> Validation String String
validateNonEmpty fieldName "" = Failure [fieldName ++ " cannot be empty"]
validateNonEmpty _ value = Success value

validateEmail :: String -> Validation String String
validateEmail email
  | '@' `elem` email = Success email
  | otherwise = Failure ["Invalid email address"]

validateAge :: Int -> Validation String Int
validateAge age
  | age < 0 = Failure ["Age cannot be negative"]
  | age > 150 = Failure ["Age seems unrealistic"]
  | otherwise = Success age

-- Combined validation
validatePerson :: String -> String -> Int -> Validation String Person
validatePerson name email age =
  Person
    <$> validateNonEmpty "Name" name
    <*> validateEmail email
    <*> validateAge age

-- ============================================
-- Main: Demonstrations
-- ============================================

main :: IO ()
main = do
  putStrLn "=== Advanced Functional Programming Patterns ==="
  
  -- 1. Lenses demonstration
  putStrLn "\n1. Lenses Example:"
  let person = Person "John Doe" 30 (Address "123 Main St" "Anytown" "12345")
      modified = over (address . city) (map toUpper) person
  print person
  print modified
  putStrLn $ "Street via lens: " ++ view personStreet person
  
  -- 2. Free monad calculator
  putStrLn "\n2. Free Monad Calculator DSL:"
  let calculation = do
        a <- add 10 5
        b <- multiply a 2
        divide b 3
  case interpretCalculator calculation of
    Left err -> putStrLn $ "Error: " ++ err
    Right result -> putStrLn $ "Result: " ++ show result
  
  -- 3. Tagless final evaluation
  putStrLn "\n3. Tagless Final Evaluation:"
  print $ runEval expr1
  print $ runEval expr2
  putStrLn $ "Pretty printed: " ++ render expr1
  
  -- 4. Yoneda/Codensity
  putStrLn "\n4. Yoneda/Codensity Optimization:"
  let result = reset (reassociateList [1, 2, 3])
  putStrLn $ "Reassociated list sum: " ++ show (take 5 result)
  
  -- 5. CPS factorial
  putStrLn "\n5. Continuation Passing Style:"
  putStrLn $ "Factorial via CPS: " ++ show (reset (factorialCPS 5))
  
  -- 6. Comonad cellular automata
  putStrLn "\n6. Comonad Cellular Automata:"
  let initial = Zipper [False, False] True [False, False]
      next = extend (cellularRule rule30) initial
  putStrLn $ "Initial: " ++ show initial
  putStrLn $ "Next generation: " ++ show (extract next)
  
  -- 7. Validation example
  putStrLn "\n7. Validation with Applicative:"
  case validatePerson "Alice" "alice@example.com" 25 of
    Success p -> putStrLn $ "Valid person: " ++ show p
    Failure errs -> putStrLn $ "Errors: " ++ intercalate ", " errs
  
  case validatePerson "" "invalid-email" 200 of
    Success p -> putStrLn $ "Valid person: " ++ show p
    Failure errs -> putStrLn $ "Errors: " ++ intercalate ", " errs
  
  putStrLn "\n✅ All functional patterns demonstrated!"