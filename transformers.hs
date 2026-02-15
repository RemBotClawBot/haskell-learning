{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Advanced Monad Transformers in Haskell
-- Practical examples of transformer stacks for real-world applications,
-- demonstrating layered effects and capability-based design.
module Transformers where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import System.IO

-- ============================================================
-- Part 1: Basic Transformer Stack Example
-- ============================================================

-- | A typical application stack:
--   ReaderT (configuration) 
--   → StateT (session state)
--   → ExceptT (errors)
--   → IO (real world)
type AppStack = ReaderT Config (StateT AppState (ExceptT AppError IO))

data Config = Config 
  { configDebug :: Bool
  , configMaxRetries :: Int
  , configTimeout :: Int
  } deriving (Show)

data AppState = AppState
  { stateUser :: Maybe User
  , stateRequestCount :: Int
  , stateLogs :: [LogEntry]
  } deriving (Show)

data User = User
  { userName :: Text
  , userId :: Int
  , userRole :: Role
  } deriving (Show)

data Role = Admin | UserRole | Guest deriving (Show)

data LogEntry = LogEntry
  { logTimestamp :: UTCTime
  , logLevel :: LogLevel
  , logMessage :: Text
  } deriving (Show)

data LogLevel = Debug | Info | Warning | Error deriving (Show)

data AppError
  = AuthError Text
  | ValidationError Text
  | DatabaseError Text
  | TimeoutError
  deriving (Show)

-- ============================================================
-- Part 2: Capability Type Classes
-- ============================================================

-- | Monadic logging capability
class Monad m => MonadLog m where
  logMessage :: LogLevel -> Text -> m ()

-- | Monadic database capability
class Monad m => MonadDatabase m where
  getUser :: Int -> m (Maybe User)
  saveUser :: User -> m ()

-- | Monadic configuration capability
class Monad m => MonadConfig m where
  getConfig :: m Config

-- ============================================================
-- Part 3: Concrete Implementations
-- ============================================================

-- | Run the application stack
runApp :: Config -> AppState -> AppStack a -> IO (Either AppError (a, AppState))
runApp config initialState action =
  runExceptT (runStateT (runReaderT action config) initialState)

-- Logging implementation
logToApp :: MonadIO m => LogLevel -> Text -> ReaderT Config (StateT AppState m) ()
logToApp level msg = do
  config <- ask
  when (configDebug config || level `elem` [Error, Warning]) $ do
    now <- liftIO getCurrentTime
    let entry = LogEntry now level msg
    modify $ \s -> s { stateLogs = entry : stateLogs stateLogs s }
    liftIO $ T.putStrLn $ T.pack (show level) <> ": " <> msg

-- Database implementation (simulated)
databaseGetUser :: MonadIO m => Int -> m (Maybe User)
databaseGetUser userId = do
  liftIO $ threadDelay 50000 -- Simulate DB delay
  if userId == 1
    then pure $ Just $ User "alice" 1 Admin
    else if userId == 2
      then pure $ Just $ User "bob" 2 UserRole
      else pure Nothing

databaseSaveUser :: MonadIO m => User -> m ()
databaseSaveUser user = do
  liftIO $ threadDelay 100000 -- Simulate DB delay
  liftIO $ T.putStrLn $ "Saved user: " <> userName user

-- ============================================================
-- Part 4: Lift and mtl-style type classes
-- ============================================================

-- Using mtl-style constraints
processRequest :: (MonadReader Config m, MonadState AppState m, 
                   MonadError AppError m, MonadIO m) => Int -> m User
processRequest userId = do
  -- Get configuration
  config <- ask
  logToApp Info $ "Processing request for user " <> T.pack (show userId)
  
  -- Increment request counter
  modify $ \s -> s { stateRequestCount = stateRequestCount s + 1 }
  
  -- Get user from database
  mUser <- liftIO $ databaseGetUser userId
  case mUser of
    Nothing -> throwError $ AuthError "User not found"
    Just user -> do
      -- Store user in state
      modify $ \s -> s { stateUser = Just user }
      logToApp Info $ "Authenticated user: " <> userName user
      
      -- Check timeout
      when (configTimeout config > 1000) $ do
        logToApp Warning "Timeout configuration seems high"
      
      pure user

-- ============================================================
-- Part 5: Free Monad vs Tagless Final
-- ============================================================

-- Free monad approach
data DatabaseF a
  = GetUser Int (Maybe User -> a)
  | SaveUser User (() -> a)
  deriving Functor

type DatabaseFree = Free DatabaseF

-- Tagless final approach
class Monad m => DatabaseTF m where
  getUserTF :: Int -> m (Maybe User)
  saveUserTF :: User -> m ()

-- Implementation for AppStack
instance DatabaseTF AppStack where
  getUserTF userId = do
    config <- ask
    when (configDebug config) $ 
      logToApp Debug $ "DatabaseTF.getUserTF: " <> T.pack (show userId)
    liftIO $ databaseGetUser userId
  
  saveUserTF user = do
    logToApp Info $ "DatabaseTF.saveUserTF: " <> userName user
    liftIO $ databaseSaveUser user

-- ============================================================
-- Part 6: Real-World Business Logic
-- ============================================================

-- | Complete user registration flow
registerUser :: (MonadReader Config m, MonadState AppState m,
                 MonadError AppError m, MonadIO m, DatabaseTF m) 
             => Text -> Role -> m User
registerUser name role = do
  -- Validate input
  when (T.length name < 2) $
    throwError $ ValidationError "Name too short"
  
  when (T.length name > 50) $
    throwError $ ValidationError "Name too long"
  
  -- Check if similar user exists (simplified)
  existing <- getUserTF 1 -- Check first user
  case existing of
    Just existingUser | userName existingUser == name ->
      throwError $ ValidationError "User already exists"
    _ -> pure ()
  
  -- Create new user
  let newUser = User name 99 role -- Hardcoded ID for demo
  
  -- Save to database
  saveUserTF newUser
  
  -- Update application state
  modify $ \s -> s { stateUser = Just newUser }
  
  -- Log successful registration
  logToApp Info $ "Registered new user: " <> name
  
  pure newUser

-- | User login flow
loginUser :: (MonadReader Config m, MonadState AppState m,
              MonadError AppError m, MonadIO m, DatabaseTF m)
          => Text -> m User
loginUser name = do
  -- Authenticate (simplified - check against first user)
  existing <- getUserTF 1
  case existing of
    Just user | userName user == name -> do
      modify $ \s -> s { stateUser = Just user }
      logToApp Info $ "User logged in: " <> name
      pure user
    _ -> throwError $ AuthError "Invalid credentials"

-- ============================================================
-- Part 7: Error Recovery and Retry Logic
-- ============================================================

-- | Retry operation with exponential backoff
retryWithBackoff :: (MonadIO m, MonadError AppError m)
                 => Int          -- Max retries
                 -> Int          -- Base delay (ms)
                 -> m a          -- Action to retry
                 -> m a
retryWithBackoff maxRetries baseDelay action = go 0
  where
    go attempt = do
      result <- catchError action $ \e -> do
        liftIO $ threadDelay (baseDelay * 2 ^ attempt * 1000)
        if attempt >= maxRetries - 1
          then throwError e
          else do
            logToApp Warning $ "Retry " <> T.pack (show (attempt + 1)) <> 
                             " after error: " <> T.pack (show e)
            go (attempt + 1)
      pure result

-- ============================================================
-- Part 8: Testing with Pure Interpreters
-- ============================================================

-- Pure interpreter for testing
type TestStack = ReaderT Config (StateT AppState (ExceptT AppError Identity))

runTest :: Config -> AppState -> TestStack a -> Either AppError (a, AppState)
runTest config initialState action =
  runIdentity $ runExceptT $ runStateT (runReaderT action config) initialState

-- Mock database for testing
mockDatabaseGetUser :: Int -> TestStack (Maybe User)
mockDatabaseGetUser userId = do
  state <- get
  case stateUser state of
    Just user | userId == 1 -> pure $ Just user
    _ -> pure Nothing

mockDatabaseSaveUser :: User -> TestStack ()
mockDatabaseSaveUser user = modify $ \s -> s { stateUser = Just user }

-- Test instance
instance DatabaseTF TestStack where
  getUserTF = mockDatabaseGetUser
  saveUserTF = mockDatabaseSaveUser

-- ============================================================
-- Part 9: Running Examples
-- ============================================================

initialState :: AppState
initialState = AppState
  { stateUser = Nothing
  , stateRequestCount = 0
  , stateLogs = []
  }

demoConfig :: Config
demoConfig = Config
  { configDebug = True
  , configMaxRetries = 3
  , configTimeout = 100
  }

demoApp :: AppStack ()
demoApp = do
  putStrLn "=== Monad Transformer Demo ==="
  
  -- Test processRequest
  user <- processRequest 1
  liftIO $ putStrLn $ "Authenticated: " ++ show user
  
  -- Test registration
  newUser <- registerUser "charlie" UserRole
  liftIO $ putStrLn $ "Registered: " ++ show newUser
  
  -- Test login
  loggedInUser <- loginUser "alice"
  liftIO $ putStrLn $ "Logged in: " ++ show loggedInUser
  
  -- Test retry logic
  result <- retryWithBackoff 3 50 $ do
    -- Simulate flaky operation
    count <- gets stateRequestCount
    if count < 2
      then throwError $ DatabaseError "Simulated database failure"
      else pure "Success!"
  
  liftIO $ putStrLn $ "Retry result: " ++ result
  
  -- Show final state
  finalState <- get
  liftIO $ putStrLn "\nFinal Application State:"
  liftIO $ print finalState

main :: IO ()
main = do
  putStrLn "=== Advanced Monad Transformers ==="
  
  -- Run the demo app
  result <- runApp demoConfig initialState demoApp
  
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (_, finalState) -> do
      putStrLn "\nApplication completed successfully"
      putStrLn $ "Total requests: " ++ show (stateRequestCount finalState)
      putStrLn $ "Log entries: " ++ show (length $ stateLogs finalState)
  
  -- Test pure interpreter
  putStrLn "\n=== Testing with Pure Interpreter ==="
  
  let testAction = do
        user <- registerUser "testuser" Guest
        pure user
  
  case runTest demoConfig initialState testAction of
    Left err -> putStrLn $ "Test error: " ++ show err
    Right (user, _) -> putStrLn $ "Test success: " ++ show user
  
  putStrLn "\n=== Transformer Examples Complete ==="