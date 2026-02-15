-- advanced-types.hs
-- Advanced Type System Features: Phantom Types, Existential Types, 
-- Type-level Programming, Higher-kinded Types, and Type Applications

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AdvancedTypes where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Data.Typeable (TypeRep, Typeable, typeOf)

-- ============================================
-- 1. Phantom Types: Types with unused parameters
-- ============================================

-- Phantom type parameter gives extra type safety
newtype Tagged (tag :: Symbol) a = Tagged { untag :: a }
  deriving (Show, Eq, Ord)

-- Create values with specific tags
data UserIdTag
data OrderIdTag
data SessionIdTag

type UserId = Tagged "userId" Int
type OrderId = Tagged "orderId" Int
type SessionId = Tagged "sessionId" String

-- Phantom types prevent mixing different IDs
createUserId :: Int -> UserId
createUserId = Tagged

createOrderId :: Int -> OrderId
createOrderId = Tagged

-- This won't compile: mixing UserId and OrderId
-- addIds :: UserId -> OrderId -> UserId
-- addIds (Tagged uid) (Tagged oid) = Tagged (uid + oid)

-- But we can safely extract the underlying values
extractId :: Tagged tag a -> a
extractId = untag

-- ============================================
-- 2. Existential Types: Hide type information
-- ============================================

-- Existentially quantified type hides the concrete type
data SomeShowable = forall a. Show a => SomeShowable a

instance Show SomeShowable where
  show (SomeShowable x) = "SomeShowable " ++ show x

-- Store values of different types in the same list
heterogeneousList :: [SomeShowable]
heterogeneousList =
  [ SomeShowable (42 :: Int)
  , SomeShowable "Hello, World!"
  , SomeShowable True
  , SomeShowable [1, 2, 3]
  ]

-- GADT version (more modern)
data SomeSerializable where
  SomeSerializable :: (Show a, Typeable a) => a -> SomeSerializable

instance Show SomeSerializable where
  show (SomeSerializable x) = 
    "Serializable[" ++ show (typeOf x) ++ "] " ++ show x

-- ============================================
-- 3. Type-level Programming with Type Families
-- ============================================

-- Type-level computation: type-level list length
type family Length (xs :: [Type]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

-- Type-level boolean operations
type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And 'True 'False = 'False
  And 'False 'True = 'False
  And 'False 'False = 'False

-- Dependency injection via type-level configuration
data Environment = Development | Staging | Production

type family DbConfig (env :: Environment) :: Type where
  DbConfig 'Development = String  -- Connection string
  DbConfig 'Staging = (String, Int)  -- String + port
  DbConfig 'Production = (String, Int, Bool)  -- + SSL flag

-- ============================================
-- 4. Higher-kinded Types (HKTs)
-- ============================================

-- Container-agnostic functions using HKTs
class Container c where
  type Element c :: Type
  empty :: c
  insert :: Element c -> c -> c
  toList :: c -> [Element c]

instance Container [] where
  type Element [] = a
  empty = []
  insert = (:)
  toList = id

instance Container Maybe where
  type Element Maybe = a
  empty = Nothing
  insert x _ = Just x
  toList Nothing = []
  toList (Just x) = [x]

-- Generalized fold over any container
foldContainer :: Container c => (Element c -> b -> b) -> b -> c -> b
foldContainer f z c = foldr f z (toList c)

-- ============================================
-- 5. Type Applications and Explicit Type Parameters
-- ============================================

-- Polymorphic identity function
polyId :: a -> a
polyId x = x

-- Using type applications
example1 :: Int
example1 = polyId @Int 42

example2 :: String
example2 = polyId @String "Hello"

-- Constraint with type applications
printIfShowable :: forall a. Show a => a -> IO ()
printIfShowable x = print x

-- Type-level strings with KnownSymbol
type Name = Symbol

greet :: forall (n :: Name). KnownSymbol n => Proxy n -> String
greet _ = "Hello, " ++ symbolVal (Proxy @n)

-- ============================================
-- 6. Advanced Type Class Patterns
-- ============================================

-- Multi-parameter type classes
class Convert a b where
  convert :: a -> b

instance Convert Int String where
  convert = show

instance Convert String Int where
  convert = read

instance Convert Bool Int where
  convert True = 1
  convert False = 0

-- Functional dependencies
class Pair a b c | a b -> c where
  pair :: a -> b -> c

instance Pair Int Int (Int, Int) where
  pair x y = (x, y)

instance Pair String Bool (String, Bool) where
  pair s b = (s, b)

-- ============================================
-- 7. Practical Example: Type-safe API endpoints
-- ============================================

data Method = GET | POST | PUT | DELETE

type family AllowedMethods (route :: Symbol) :: [Method] where
  AllowedMethods "users" = '[GET, POST]
  AllowedMethods "users/:id" = '[GET, PUT, DELETE]
  AllowedMethods "posts" = '[GET, POST]
  AllowedMethods "posts/:id" = '[GET, PUT, DELETE]

-- Type-safe route handler
data RouteHandler (m :: Method) (route :: Symbol) = RouteHandler
  { handle :: String -> IO String
  }

-- Example: GET /users handler
getUsersHandler :: RouteHandler GET "users"
getUsersHandler = RouteHandler $ \_ -> 
  pure "{\"users\": [\"alice\", \"bob\"]}"

-- Example: POST /users handler  
postUsersHandler :: RouteHandler POST "users"
postUsersHandler = RouteHandler $ \body ->
  pure $ "Created user with data: " ++ body

-- ============================================
-- 8. Type-safe Units of Measurement
-- ============================================

newtype Meter (a :: Type) = Meter Double
  deriving (Show, Num)

newtype Second (a :: Type) = Second Double
  deriving (Show, Num)

newtype MeterPerSecond (a :: Type) = MeterPerSecond Double
  deriving (Show)

-- Type-safe velocity calculation
velocity :: Meter a -> Second b -> MeterPerSecond (a, b)
velocity (Meter m) (Second s) = MeterPerSecond (m / s)

-- Type-safe acceleration
newtype MeterPerSecondSquared (a :: Type) = MeterPerSecondSquared Double
  deriving (Show)

accel :: MeterPerSecond a -> Second b -> MeterPerSecondSquared (a, b)
accel (MeterPerSecond v) (Second t) = MeterPerSecondSquared (v / t)

-- ============================================
-- Main: Run all examples
-- ============================================

main :: IO ()
main = do
  putStrLn "=== Advanced Type System Examples ==="
  
  -- Phantom types
  putStrLn "\n1. Phantom Types:"
  let uid = createUserId 42
      oid = createOrderId 100
  print $ "User ID: " ++ show uid
  print $ "Order ID: " ++ show oid
  print $ "Extracted user ID: " ++ show (extractId uid)
  
  -- Existential types
  putStrLn "\n2. Existential Types:"
  mapM_ print heterogeneousList
  
  -- Type applications
  putStrLn "\n3. Type Applications:"
  print (example1 :: Int)
  print (example2 :: String)
  
  -- Type-level strings
  putStrLn "\n4. Type-level Strings:"
  print $ greet (Proxy @"Alice")
  print $ greet (Proxy @"Bob")
  
  -- Multi-parameter type classes
  putStrLn "\n5. Multi-parameter Type Classes:"
  print $ convert (42 :: Int) :: String
  print $ convert "99" :: Int
  print $ convert True :: Int
  
  putStrLn "\n✅ Advanced type system examples completed!"