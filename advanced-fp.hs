-- advanced-fp.hs
-- Advanced Functional Programming: Profunctors, Optics, Effect Systems, and Algebraic Abstractions
-- Demonstrates advanced type classes, functional patterns, and modern Haskell techniques

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AdvancedFP where

import Control.Arrow (Kleisli(..))
import Control.Monad.Free (Free(..), liftF)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Profunctor (Profunctor(..), Star(..))
import Data.Profunctor.Choice (Choice(..))
import Data.Profunctor.Strong (Strong(..))
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================
-- 1. Profunctors & Kleisli Arrows
-- ============================================

-- Profunctor instance for Kleisli arrows gives us dimap/lmap/rmap for effectful functions
instance Monad m => Profunctor (Kleisli m) where
  dimap ab cd (Kleisli bc) = Kleisli $ \a' -> do
    b <- bc (ab a')
    pure (cd b)

-- Helper to show how profunctor composition works
sanitize :: Text -> Text
sanitize = T.strip . T.toLower

validateLength :: Kleisli Maybe Text Text
validateLength = Kleisli $ \txt ->
  let trimmed = sanitize txt
  in if T.length trimmed >= 3 then Just trimmed else Nothing

renderGreeting :: Kleisli Maybe Text String
renderGreeting = dimap id (
  \name -> "Hello, " ++ name ++ "!") validateLength

-- ============================================
-- 2. Optics with Profunctor Lenses/Prisms
-- ============================================

type Lens s t a b = forall p. Strong p => p a b -> p s t
type Lens' s a = Lens s s a a

type Prism s t a b = forall p. Choice p => p a b -> p s t
type Prism' s a = Prism s s a a

view :: Lens' s a -> s -> a
view l s = getConst $ runStar (l (Star Const)) s

set :: Lens s t a b -> b -> s -> t
set l b s = runIdentity $ runStar (l (Star (const (Identity b)))) s

over :: Lens s t a b -> (a -> b) -> s -> t
over l f s = runIdentity $ runStar (l (Star (Identity . f))) s

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter pab = dimap to from (first' pab)
  where
    to s = (getter s, s)
    from (b, s) = setter s b

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism build match pab = dimap match (either id build) (right' pab)

_1 :: Lens (a, c) (b, c) a b
_1 = lens fst (\(_, c) b -> (b, c))

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)

-- ============================================
-- 3. Free Monads & Tagless Interpreters
-- ============================================

data ConsoleF next
  = Ask (String -> next)
  | Tell String next
  deriving Functor

type ConsoleProgram = Free ConsoleF

ask :: ConsoleProgram String
ask = liftF (Ask id)

tell :: String -> ConsoleProgram ()
tell msg = liftF (Tell msg ())

survey :: ConsoleProgram ()
survey = do
  tell "What is your name?"
  name <- ask
  tell $ "Nice to meet you, " ++ name ++ "!"

interpretIO :: ConsoleProgram a -> IO a
interpretIO (Pure a) = pure a
interpretIO (Free (Tell msg next)) = putStrLn msg >> interpretIO next
interpretIO (Free (Ask k)) = getLine >>= interpretIO . k

runConsoleState :: [String] -> ConsoleProgram a -> (a, [String], [String])
runConsoleState inputs program =
  let (result, (out, remaining)) = go program ([], inputs)
  in (result, reverse out, remaining)
  where
    go (Pure a) st = (a, st)
    go (Free (Tell msg next)) (out, inp) = go next (msg:out, inp)
    go (Free (Ask k)) (out, inp) =
      case inp of
        [] -> go (k "") (out, [])
        (x:xs) -> go (k x) (out, xs)

-- ============================================
-- 4. Contravariant Structures
-- ============================================

newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)

isEven :: Predicate Int
isEven = Predicate even

greaterThan :: Int -> Predicate Int
greaterThan n = Predicate (> n)

longerThan :: Int -> Predicate String
longerThan n = contramap length (greaterThan n)

-- ============================================
-- 5. Comonads & Zippers
-- ============================================

data Zipper a = Zipper [a] a [a]
  deriving (Eq, Show)

moveLeft :: Zipper a -> Maybe (Zipper a)
moveLeft (Zipper [] _ _) = Nothing
moveLeft (Zipper (l:ls) x rs) = Just (Zipper ls l (x:rs))

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Zipper _ _ []) = Nothing
moveRight (Zipper ls x (r:rs)) = Just (Zipper (x:ls) r rs)

extend :: (Zipper a -> b) -> Zipper a -> Zipper b
extend f z = Zipper (unfold moveLeft z) (f z) (unfold moveRight z)
  where
    unfold shift seed = maybe [] (\z' -> f z' : unfold shift z') (shift seed)

rule30 :: Zipper Bool -> Bool
rule30 z = case neighborhood of
  (True, True, True)     -> False
  (True, True, False)    -> False
  (True, False, True)    -> False
  (True, False, False)   -> True
  (False, True, True)    -> True
  (False, True, False)   -> True
  (False, False, True)   -> True
  (False, False, False)  -> False
  where
    leftVal  = maybe False current (moveLeft z)
    currVal  = current z
    rightVal = maybe False current (moveRight z)
    current (Zipper _ x _) = x
    neighborhood = (leftVal, currVal, rightVal)

-- ============================================
-- 6. Recursion Schemes
-- ============================================

newtype Fix f = Fix { unFix :: f (Fix f) }

data TreeF a r = Leaf | Node a r r
  deriving Functor

type Tree a = Fix (TreeF a)

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

listToTree :: [a] -> Tree a
listToTree = ana coalg
  where
    coalg [] = Leaf
    coalg (x:xs) = Node x (take half xs) (drop half xs)
      where half = length xs `div` 2

treeSum :: Tree Int -> Int
treeSum = cata $ \case
  Leaf -> 0
  Node x l r -> x + l + r

-- ============================================
-- 7. Type-Level Programming
-- ============================================

data Nat = Z | S Nat

data Vec :: Nat -> Type -> Type where
  VNil :: Vec 'Z a
  VCons :: a -> Vec n a -> Vec ('S n) a

type family Add (m :: Nat) (n :: Nat) :: Nat where
  Add 'Z n = n
  Add ('S m) n = 'S (Add m n)

vhead :: Vec ('S n) a -> a
vhead (VCons x _) = x

vappend :: Vec m a -> Vec n a -> Vec (Add m n) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)

-- ============================================
-- 8. Demo Runner
-- ============================================

main :: IO ()
main = do
  putStrLn "===== Advanced FP Tour ====="
  putStrLn "\n1) Profunctor validation demo:"
  print $ runKleisli renderGreeting "  Alice  "
  print $ runKleisli renderGreeting "xy"

  putStrLn "\n2) Lens manipulation:"
  print $ over _1 (+1) (41, "answer")
  print $ view _1 (99, "bottles")

  putStrLn "\n3) Free monad survey (state interpreter):"
  let ((), logs, _) = runConsoleState ["Rem"] survey
  putStrLn $ "Console transcript: " ++ intercalate " | " logs

  putStrLn "\n4) Contravariant predicate composition:"
  let Predicate p = longerThan 5
  print (p "Haskell")
  print (p "abc")

  putStrLn "\n5) Recursion scheme tree sum:"
  let tree = listToTree [1..7]
  print (treeSum tree)

  putStrLn "\n6) Type-safe vectors concatenate at the type level:"
  let vecA = VCons 'a' (VCons 'b' VNil)
      vecB = VCons 'c' VNil
  print (vhead vecA, vhead vecB)
  putStrLn "Manual inspection assures lengths add via Add type family."

  putStrLn "\nAll advanced demos complete."
