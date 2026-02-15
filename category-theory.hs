-- category-theory.hs
-- Category Theory Foundations for Haskell
-- Demonstrates categorical concepts and their Haskell implementations
-- Essential for understanding monads, functors, applicatives, and more

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module CategoryTheory where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (join, (>=>))
import Data.Function (fix, (&))
import Data.Kind (Constraint, Type)
import Data.List (intercalate)
import Prelude hiding (Functor, fmap, id, (.))

-- ============================================
-- 1. Basic Category Definitions
-- ============================================

-- | Category: objects and arrows (morphisms) with identity and composition
class Category (cat :: k -> k -> Type) where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

-- | Hask: The category where objects are Haskell types 
--   and morphisms are functions between them
instance Category (->) where
  id x = x
  (f . g) x = f (g x)

-- ============================================
-- 2. Functors: Mappings Between Categories
-- ============================================

-- | Functor preserves identity and composition
class (Category c, Category d) => Functor' (c :: i -> i -> Type)
                                          (d :: j -> j -> Type)
                                          (f :: i -> j) where
  fmap' :: c a b -> d (f a) (f b)

-- Haskell's Functor is endofunctors on Hask
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Laws:
-- 1. fmap id = id
-- 2. fmap (f . g) = fmap f . fmap g

data Maybe' a = Nothing' | Just' a deriving (Show, Eq)

instance Functor Maybe' where
  fmap f Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

-- Verifying functor laws
testFunctorLaws :: IO ()
testFunctorLaws = do
  putStrLn "Testing Maybe' Functor Laws:"
  
  -- Law 1: fmap id = id
  let law1_1 = fmap id (Nothing' :: Maybe' Int) == id (Nothing' :: Maybe' Int)
      law1_2 = fmap id (Just' 42) == id (Just' 42)
  
  -- Law 2: fmap (f . g) = fmap f . fmap g
  let f = (+1)
      g = (*2)
      law2_1 = fmap (f . g) Nothing' == (fmap f . fmap g) Nothing'
      law2_2 = fmap (f . g) (Just' 5) == (fmap f . fmap g) (Just' 5)
  
  mapM_ (\(label, result) -> putStrLn $ label ++ ": " ++ show result) $
    [ ("Law 1 - Nothing", law1_1)
    , ("Law 1 - Just", law1_2)
    , ("Law 2 - Nothing", law2_1)
    , ("Law 2 - Just", law2_2)
    ]

-- ============================================
-- 3. Natural Transformations
-- ============================================

-- | Natural transformation between functors F and G
type Nat f g = forall x. f x -> g x

-- Example: maybeToList as natural transformation
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- Naturality condition:
--   for any f :: a -> b, maybeToList . fmap f = fmap f . maybeToList

-- ============================================
-- 4. Monads as Monoids in the Category of Endofunctors
-- ============================================

-- | Kleisli category presentation
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Category (Kleisli m) where
  id = Kleisli return
  Kleisli f . Kleisli g = Kleisli (g >=> f)

-- | Monad from categorical perspective
class Functor m => Monad' m where
  return' :: a -> m a
  join'   :: m (m a) -> m a

-- Standard monad from categorical primitives
instance Monad' Maybe where
  return' = Just
  join' Nothing = Nothing
  join' (Just mx) = mx

-- ============================================
-- 5. Adjunctions and Free/Forgetful Functors
-- ============================================

-- | Free monoid functor (list)
data FreeMonoid a = FreeMonoid { getList :: [a] }

instance Functor FreeMonoid where
  fmap f (FreeMonoid xs) = FreeMonoid (map f xs)

instance Applicative FreeMonoid where
  pure x = FreeMonoid [x]
  FreeMonoid fs <*> FreeMonoid xs = FreeMonoid (fs <*> xs)

instance Monad FreeMonoid where
  return = pure
  FreeMonoid xs >>= f = FreeMonoid $ concatMap (getList . f) xs

-- | Forgetful functor from monoids to sets
forget :: Monoid m => m -> ()
forget _ = ()

-- Adjunction: Free ⊣ Forget

-- ============================================
-- 6. Monoidal Categories and Applicatives
-- ============================================

-- | Product type as monoidal structure
data Product a b = Product a b deriving (Show, Eq)

instance Functor (Product a) where
  fmap f (Product x y) = Product x (f y)

-- | Day convolution for applicative functors
data Day f g a where
  Day :: f b -> g c -> (b -> c -> a) -> Day f g a

instance (Functor f, Functor g) => Functor (Day f g) where
  fmap f (Day fb gc bca) = Day fb gc (\b c -> f (bca b c))

-- Representing Applicative via Day convolution
class Functor f => Applicative' f where
  pure' :: a -> f a
  day' :: Day f f a -> f a

-- ============================================
-- 7. Comonads
-- ============================================

-- | Comonad: dual to monad
class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

-- Non-empty list as comonad
data NonEmpty a = a :| [a] deriving (Show, Eq)

instance Functor NonEmpty where
  fmap f (x :| xs) = f x :| map f xs

instance Comonad NonEmpty where
  extract (x :| _) = x
  duplicate ne@(x :| xs) = (x :| xs) :| zipWith (:|) (drop 1 xs) (tails xs)
    where tails [] = []
          tails (_:ys) = tails ys

-- ============================================
-- 8. Yoneda Lemma Implementation
-- ============================================

-- | Yoneda embedding
newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

instance Functor (Yoneda f) where
  fmap f (Yoneda g) = Yoneda (\h -> g (h . f))

-- | Yoneda lemma: Yoneda f a ≅ f a
toYoneda :: Functor f => f a -> Yoneda f a
toYoneda fa = Yoneda (\f -> fmap f fa)

fromYoneda :: Yoneda f a -> f a
fromYoneda (Yoneda g) = g id

-- ============================================
-- 9. Practical Examples
-- ============================================

-- | Reader monad as function
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure x = Reader (\_ -> x)
  Reader f <*> Reader g = Reader (\r -> f r (g r))

instance Monad (Reader r) where
  Reader g >>= f = Reader $ \r -> runReader (f (g r)) r

-- | State comonad
newtype Store s a = Store (s -> a, s)

instance Functor (Store s) where
  fmap f (Store (g, s)) = Store (f . g, s)

instance Comonad (Store s) where
  extract (Store (f, s)) = f s
  duplicate (Store (f, s)) = Store (\s' -> Store (f, s'), s)

-- ============================================
-- 10. Category Theory in Practice
-- ============================================

-- | Lens as getter/setter pair (profunctor optics)
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- Example lens for tuple first element
_1 :: Lens (a, c) (b, c) a b
_1 f (a, c) = (\b -> (b, c)) <$> f a

-- | Traversal (multi-focus optic)
type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t

-- ============================================
-- Exercises Section
-- ============================================

exercises :: IO ()
exercises = do
  putStrLn "\n=== Category Theory Exercises ===\n"
  
  putStrLn "1. Implement the opposite category (reverse arrows):"
  putStrLn "   data Op cat a b = Op { unOp :: cat b a }"
  putStrLn "   instance Category cat => Category (Op cat)"
  
  putStrLn "\n2. Show that lists form a free monoid:"
  putStrLn "   Prove: FreeMonoid a ≅ [a]"
  
  putStrLn "\n3. Implement the Contravariant typeclass:"
  putStrLn "   class Contravariant f where"
  putStrLn "     contramap :: (b -> a) -> f a -> f b"
  
  putStrLn "\n4. Demonstrate naturality condition for Maybe ~> []:"
  putStrLn "   maybeToList . fmap f = fmap f . maybeToList"
  
  putStrLn "\n5. Implement Applicative via Day convolution:"
  putStrLn "   pure via identity, <*> via zip-like operation"
  
  putStrLn "\n6. Prove Yoneda lemma for Maybe:"
  putStrLn "   Yoneda Maybe a ≅ Maybe a"

main :: IO ()
main = do
  testFunctorLaws
  
  putStrLn "\n=== Category Theory Examples ==="
  
  -- Functor example
  let maybeExample = fmap (*2) (Just' 21)
  putStrLn $ "Functor example: fmap (*2) (Just' 21) = " ++ show maybeExample
  
  -- Natural transformation
  let natExample1 = maybeToList (Just "hello")
      natExample2 = maybeToList (fmap length (Just "world"))
  putStrLn $ "Natural transformation: maybeToList (Just \"hello\") = " ++ show natExample1
  putStrLn $ "fmap length then maybeToList: " ++ show natExample2
  
  -- Kleisli category
  let addKleisli :: Kleisli Maybe Int Int
      addKleisli = Kleisli (\x -> if x > 0 then Just (x + 1) else Nothing)
      mulKleisli :: Kleisli Maybe Int Int
      mulKleisli = Kleisli (\x -> if x /= 0 then Just (x * 2) else Nothing)
      composed = mulKleisli . addKleisli
  putStrLn $ "Kleisli composition: runKleisli composed 3 = " ++ show (runKleisli composed 3)
  
  -- Comonad example
  let ne = 1 :| [2, 3, 4]
  putStrLn $ "Comonad extract: extract (1 :| [2,3,4]) = " ++ show (extract ne)
  putStrLn $ "Comonad duplicate head: head (extract (duplicate ne)) = " ++ 
    show (extract . head . getList $ duplicate ne)
    where getList (x :| xs) = x:xs
  
  exercises