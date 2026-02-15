-- effect-systems.hs
-- Building practical effect systems with monad transformers and type classes
-- Focuses on ReaderT pattern, ExceptT error handling, structured logging, and
-- pure interpreters for easier testing.

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module EffectSystems where

import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import System.Random (randomRIO)

-- ============================================
-- 1. Application Environment & Errors
-- ============================================

data Env = Env
  { envServiceName :: Text
  , envConfig :: Map Text Text
  , envNow :: IO UTCTime
  }

data AppError
  = NotFound Text
  | ValidationFailed Text
  | ExternalFailure Text
  deriving (Show, Eq)

-- ============================================
-- 2. Type Classes for Capabilities
-- ============================================

class Monad m => HasConfig m where
  getConfig :: Text -> m (Maybe Text)

class Monad m => HasClock m where
  now :: m UTCTime

class Monad m => HasRandom m where
  nextInt :: (Int, Int) -> m Int

class Monad m => HasLog m where
  logLine :: Text -> m ()

-- ============================================
-- 3. Concrete Effect Stack
-- ============================================

newtype AppM a = AppM
  { runAppM :: ReaderT Env (ExceptT AppError (WriterT [Text] IO)) a }
  deriving newtype (Functor, Applicative, Monad, MonadReader Env,
                    MonadError AppError, MonadWriter [Text], MonadIO)

instance HasConfig AppM where
  getConfig key = do
    Env{..} <- ask
    pure $ M.lookup key envConfig

instance HasClock AppM where
  now = do
    Env{..} <- ask
    liftIO envNow

instance HasRandom AppM where
  nextInt bounds = liftIO (randomRIO bounds)

instance HasLog AppM where
  logLine msg = tell [msg]

-- ============================================
-- 4. Core Business Logic (Pure, Capability Driven)
-- ============================================

fetchApiKey :: (HasConfig m, MonadError AppError) => m Text
fetchApiKey = do
  value <- getConfig "API_KEY"
  case value of
    Nothing -> throwError (NotFound "API_KEY")
    Just k  -> pure k

recordMetric :: (HasLog m, HasClock m) => Text -> m ()
recordMetric metric = do
  ts <- now
  logLine $ metric <> " at " <> T.pack (show ts)

chargeCustomer :: (HasRandom m, HasLog m, MonadError AppError)
               => Int -> m Int
chargeCustomer cents = do
  response <- nextInt (1, 10)
  if response <= 8
    then logLine ("Charged " <> T.pack (show cents) <> " cents") >> pure cents
    else throwError (ExternalFailure "Payment processor offline")

checkoutFlow :: (HasConfig m, HasClock m, HasRandom m, HasLog m, MonadError AppError)
             => Int -> m Text
checkoutFlow cents = do
  apiKey <- fetchApiKey
  recordMetric "checkout.start"
  charged <- chargeCustomer cents
  recordMetric "checkout.success"
  pure $ "Charged " <> T.pack (show charged) <> " using key " <> apiKey

-- ============================================
-- 5. Pure Interpreter for Testing
-- ============================================

data TestState = TestState
  { tsConfig :: Map Text Text
  , tsRandom :: [Int]
  , tsLog :: [Text]
  , tsTime :: UTCTime
  }

newtype TestM a = TestM { runTestM :: StateT TestState (Either AppError) a }
  deriving newtype ( Functor
                   , Applicative
                   , Monad
                   , MonadState TestState
                   , MonadError AppError
                   )

data TestResult a = TestResult
  { trValue :: Either AppError a
  , trLogs  :: [Text]
  } deriving (Show)

runTest :: TestState -> TestM a -> TestResult a
runTest initial (TestM action) =
  case runStateT action initial of
    Left err          -> TestResult (Left err) (tsLog initial)
    Right (value, st) -> TestResult (Right value) (tsLog st)

instance HasConfig TestM where
  getConfig key = do
    cfg <- gets tsConfig
    pure (M.lookup key cfg)

instance HasClock TestM where
  now = gets tsTime

instance HasRandom TestM where
  nextInt _ = do
    nums <- gets tsRandom
    case nums of
      (n:rest) -> modify' (\s -> s { tsRandom = rest }) >> pure n
      []       -> throwError (ValidationFailed "Random stream exhausted")

instance HasLog TestM where
  logLine msg = modify' (\s -> s { tsLog = tsLog s <> [msg] })

-- ============================================
-- 6. Running the App
-- ============================================

runProd :: Env -> AppM a -> IO (Either AppError a, [Text])
runProd env action = runWriterT (runExceptT (runReaderT (runAppM action) env))

sampleEnv :: IO Env
sampleEnv = do
  nowFn <- pure getCurrentTime
  pure $ Env
    { envServiceName = "checkout"
    , envConfig = M.fromList [("API_KEY", "live-123"),("REGION","us-east")]
    , envNow = nowFn
    }

-- ============================================
-- 7. Demonstration Scenarios
-- ============================================

successScenario :: IO ()
successScenario = do
  env <- sampleEnv
  (result, logs) <- runProd env (checkoutFlow 4999)
  putStrLn "=== Success Scenario ==="
  print result
  mapM_ (putStrLn . T.unpack) logs

failureScenario :: IO ()
failureScenario = do
  env <- sampleEnv
  let flakyEnv = env { envConfig = M.delete "API_KEY" (envConfig env) }
  (result, logs) <- runProd flakyEnv (checkoutFlow 4999)
  putStrLn "\n=== Failure Scenario ==="
  print result
  mapM_ (putStrLn . T.unpack) logs

pureTestScenario :: IO ()
pureTestScenario = do
  timestamp <- getCurrentTime
  let initial = TestState
        { tsConfig = M.fromList [("API_KEY", "test-xyz")]
        , tsRandom = cycle [1,2,3]
        , tsLog = []
        , tsTime = timestamp
        }
      TestResult{..} = runTest initial (checkoutFlow 1200)
  putStrLn "\n=== Pure Interpreter Scenario ==="
  print trValue
  mapM_ (putStrLn . T.unpack) trLogs

-- ============================================
-- 8. Exercises
-- ============================================

exercises :: IO ()
exercises = do
  putStrLn "\n=== Effect System Exercises ==="
  putStrLn "1. Add caching via StateT to memoize API responses"
  putStrLn "2. Provide a pure interpreter that never throws by returning Either"
  putStrLn "3. Implement retry logic using ExceptT and WriterT"
  putStrLn "4. Generalize HasConfig to support typed values"
  putStrLn "5. Add tracing IDs via ReaderT context"

-- ============================================
-- 9. Main Entry
-- ============================================

main :: IO ()
main = do
  successScenario
  failureScenario
  pureTestScenario
  exercises
