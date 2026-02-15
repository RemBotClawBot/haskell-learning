-- web-api.hs
-- Building Web APIs with Haskell: Warp, Aeson, and practical patterns
-- Demonstrates REST API creation, JSON parsing, middleware, and database integration patterns

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebAPI where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200, status201, status400, status404)
import Network.Wai (Application, Request, Response, pathInfo, requestMethod, responseLBS, strictRequestBody)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse (FileInfo(..), lbsBackEnd, parseRequestBody)

-- ============================================
-- 1. Data Models with Aeson Deriving
-- ============================================

data Todo = Todo
  { todoId :: Int
  , task :: Text
  , completed :: Bool
  , createdAt :: UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON Todo
instance ToJSON Todo

data User = User
  { userId :: Int
  , username :: Text
  , email :: Text
  , age :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON User
instance ToJSON User

data ApiError = ApiError
  { errorCode :: Int
  , message :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON ApiError
instance ToJSON ApiError

-- Helper for creating API responses
apiResponse :: ToJSON a => Int -> a -> BL.ByteString
apiResponse status payload = encode $ object
  [ "success" .= (status >= 200 && status < 300)
  , "data" .= payload
  , "status" .= status
  ]

apiError :: Int -> Text -> BL.ByteString
apiError status msg = apiResponse status (ApiError status msg)

-- ============================================
-- 2. In-Memory Database Simulation
-- ============================================

type TodoStore = MVar [Todo]

initialStore :: IO TodoStore
initialStore = newMVar []

modifyStore :: TodoStore -> ([Todo] -> ([Todo], a)) -> IO a
modifyStore store f = modifyMVar store $ \todos ->
  let (updated, result) = f todos
  in pure (updated, result)

getTodos :: TodoStore -> IO [Todo]
getTodos = readMVar

getTodoById :: TodoStore -> Int -> IO (Maybe Todo)
getTodoById store ident = do
  todos <- getTodos store
  pure $ go todos
  where
    go [] = Nothing
    go (t:ts)
      | todoId t == ident = Just t
      | otherwise = go ts

addTodo :: TodoStore -> Todo -> IO Todo
addTodo store todo = modifyStore store (\todos -> (todo : todos, todo))

updateTodo :: TodoStore -> Int -> (Todo -> Todo) -> IO (Maybe Todo)
updateTodo store ident updater = modifyStore store (go ident)
  where
    go _ [] = ([], Nothing)
    go n (t:ts)
      | todoId t == n =
          let updated = updater t
          in (updated : ts, Just updated)
      | otherwise =
          let (rest, result) = go n ts
          in (t : rest, result)

deleteTodo :: TodoStore -> Int -> IO Bool
deleteTodo store ident = modifyStore store (go ident)
  where
    go _ [] = ([], False)
    go n (t:ts)
      | todoId t == n = (ts, True)
      | otherwise =
          let (rest, result) = go n ts
          in (t : rest, result)

-- ============================================
-- 3. Request Parsing and Validation
-- ============================================

data CreateTodoRequest = CreateTodoRequest
  { taskText :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreateTodoRequest
instance ToJSON CreateTodoRequest

data UpdateTodoRequest = UpdateTodoRequest
  { newTask :: Maybe Text
  , newCompleted :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateTodoRequest
instance ToJSON UpdateTodoRequest

validateCreateTodo :: CreateTodoRequest -> Either Text CreateTodoRequest
validateCreateTodo req
  | T.null (taskText req) = Left "Task text cannot be empty"
  | T.length (taskText req) > 1000 = Left "Task text too long (max 1000 chars)"
  | otherwise = Right req

nextId :: [Todo] -> Int
nextId [] = 1
nextId todos = maximum (map todoId todos) + 1

-- ============================================
-- 4. Middleware Components
-- ============================================

loggingMiddleware :: Application -> Application
loggingMiddleware app req respond = do
  startTime <- getCurrentTime
  app req $ \response -> do
    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ unwords
      [ T.unpack (TE.decodeUtf8 (requestMethod req))
      , show (pathInfo req)
      , "took"
      , show diff
      ]
    respond response

corsMiddleware :: Application -> Application
corsMiddleware app req respond = do
  let addCorsHeaders = Wai.mapResponseHeaders (\headers ->
        ("Access-Control-Allow-Origin", "*") :
        ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS") :
        ("Access-Control-Allow-Headers", "Content-Type") :
        headers)
  if requestMethod req == "OPTIONS"
    then respond $ responseLBS status200
      [ ("Access-Control-Allow-Origin", "*")
      , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
      , ("Access-Control-Allow-Headers", "Content-Type")
      ]
      "OK"
    else app req (respond . addCorsHeaders)

-- ============================================
-- 5. Main Application with Route Handlers
-- ============================================

router :: TodoStore -> Application
router store req respond = case pathInfo req of
  ["health"] -> healthCheckApp store req respond
  ["ready"]  -> healthCheckApp store req respond
  ["upload"] -> fileUploadHandler req respond
  _           -> todoApp store req respond

-- Todo routes

todoApp :: TodoStore -> Application
todoApp store req respond = do
  response <- case (requestMethod req, pathInfo req) of
    ("GET", []) -> getTodosHandler store
    ("GET", [segment]) -> getTodoHandler store (parseId segment)
    ("POST", []) -> createTodoHandler store req
    ("PUT", [segment]) -> updateTodoHandler store req (parseId segment)
    ("DELETE", [segment]) -> deleteTodoHandler store (parseId segment)
    _ -> pure $ responseLBS status404 [json] (apiError 404 "Route not found")
  respond response
  where
    json = ("Content-Type", "application/json")
    parseId textValue = case reads (T.unpack textValue) of
      [(n, "")] -> Just n
      _ -> Nothing

getTodosHandler :: TodoStore -> IO Response
getTodosHandler store = do
  todos <- getTodos store
  pure $ responseLBS status200 [("Content-Type", "application/json")] (apiResponse 200 todos)

getTodoHandler :: TodoStore -> Maybe Int -> IO Response
getTodoHandler _ Nothing = pure $ responseLBS status400 [("Content-Type", "application/json")] (apiError 400 "Invalid ID format")
getTodoHandler store (Just ident) = do
  result <- getTodoById store ident
  pure $ case result of
    Just todo -> responseLBS status200 [("Content-Type", "application/json")] (apiResponse 200 todo)
    Nothing   -> responseLBS status404 [("Content-Type", "application/json")] (apiError 404 "Todo not found")

createTodoHandler :: TodoStore -> Request -> IO Response
createTodoHandler store req = do
  body <- strictRequestBody req
  case decode body of
    Nothing -> pure $ responseLBS status400 [("Content-Type", "application/json")] (apiError 400 "Invalid JSON")
    Just createReq -> case validateCreateTodo createReq of
      Left err -> pure $ responseLBS status400 [("Content-Type", "application/json")] (apiError 400 err)
      Right CreateTodoRequest{..} -> do
        now <- getCurrentTime
        todo <- modifyStore store $ \todos ->
          let newId = nextId todos
              newTodo = Todo newId taskText False now
          in (newTodo : todos, newTodo)
        pure $ responseLBS status201 [("Content-Type", "application/json")] (apiResponse 201 todo)

updateTodoHandler :: TodoStore -> Request -> Maybe Int -> IO Response
updateTodoHandler _ _ Nothing = pure $ responseLBS status400 [("Content-Type", "application/json")] (apiError 400 "Invalid ID format")
updateTodoHandler store req (Just ident) = do
  body <- strictRequestBody req
  case decode body of
    Nothing -> pure $ responseLBS status400 [("Content-Type", "application/json")] (apiError 400 "Invalid JSON")
    Just UpdateTodoRequest{..} -> do
      result <- updateTodo store ident $ \todo -> todo
        { task = fromMaybe (task todo) newTask
        , completed = fromMaybe (completed todo) newCompleted
        }
      pure $ case result of
        Just updated -> responseLBS status200 [("Content-Type", "application/json")] (apiResponse 200 updated)
        Nothing      -> responseLBS status404 [("Content-Type", "application/json")] (apiError 404 "Todo not found")

deleteTodoHandler :: TodoStore -> Maybe Int -> IO Response
deleteTodoHandler _ Nothing = pure $ responseLBS status400 [("Content-Type", "application/json")] (apiError 400 "Invalid ID format")
deleteTodoHandler store (Just ident) = do
  deleted <- deleteTodo store ident
  pure $ if deleted
    then responseLBS status200 [("Content-Type", "application/json")] (apiResponse 200 (object ["deleted" .= True]))
    else responseLBS status404 [("Content-Type", "application/json")] (apiError 404 "Todo not found")

-- ============================================
-- 6. File Upload Example (Multipart forms)
-- ============================================

fileUploadHandler :: Application
fileUploadHandler req respond =
  if requestMethod req == "POST"
    then do
      (_, files) <- parseRequestBody lbsBackEnd req
      case lookup "file" files of
        Nothing -> respond $ responseLBS status400 [("Content-Type", "text/plain")] "No file uploaded"
        Just fileInfo -> do
          let filenameTxt = TE.decodeUtf8 (fileName fileInfo)
              fileSize = BL.length (fileContent fileInfo)
              payload = object
                [ "filename" .= filenameTxt
                , "size" .= fileSize
                , "success" .= True
                ]
          respond $ responseLBS status200 [("Content-Type", "application/json")] (encode payload)
    else respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not found"

-- ============================================
-- 7. Health Check and Monitoring Endpoints
-- ============================================

healthCheckApp :: TodoStore -> Application
healthCheckApp store req respond = do
  response <- case pathInfo req of
    ["health"] -> do
      count <- length <$> getTodos store
      timestamp <- getCurrentTime
      pure $ responseLBS status200 [("Content-Type", "application/json")] (encode $ object
        [ "status" .= ("healthy" :: Text)
        , "timestamp" .= timestamp
        , "todoCount" .= count
        ])
    ["ready"] -> pure $ responseLBS status200 [("Content-Type", "application/json")] (encode $ object ["ready" .= True])
    _ -> pure $ responseLBS status404 [("Content-Type", "application/json")] (apiError 404 "Not found")
  respond response

-- ============================================
-- 8. Main Entry Point and Server Initialization
-- ============================================

main :: IO ()
main = do
  putStrLn "Starting Todo API server on port 8080..."
  store <- initialStore
  now <- getCurrentTime
  let starter =
        [ Todo 1 "Learn Haskell" False now
        , Todo 2 "Build a web API" False now
        , Todo 3 "Test the API" True now
        ]
  modifyMVar_ store (const (pure starter))
  let baseApp = router store
      app = corsMiddleware $ loggingMiddleware baseApp
  putStrLn "Available endpoints:"
  mapM_ putStrLn
    [ "  GET    /           - List all todos"
    , "  GET    /:id        - Fetch todo by id"
    , "  POST   /           - Create todo"
    , "  PUT    /:id        - Update todo"
    , "  DELETE /:id        - Delete todo"
    , "  GET    /health     - Health check"
    , "  GET    /ready      - Ready check"
    , "  POST   /upload     - File upload demo"
    ]
  run 8080 app

-- ============================================
-- 9. Quick Test Runner
-- ============================================

runTests :: IO ()
runTests = do
  putStrLn "Running API store tests..."
  store <- initialStore
  todos0 <- getTodos store
  putStrLn $ "Initial store empty: " ++ show (null todos0)

  now <- getCurrentTime
  let sample = Todo 1 "Test task" False now
  _ <- addTodo store sample
  retrieved <- getTodoById store 1
  putStrLn $ "Retrieved todo: " ++ show retrieved

  _ <- updateTodo store 1 (\t -> t { completed = True })
  updated <- getTodoById store 1
  putStrLn $ "Updated todo completed field: " ++ show (fmap completed updated)

  deleted <- deleteTodo store 1
  putStrLn $ "Deleted todo: " ++ show deleted

  finalTodos <- getTodos store
  putStrLn $ "Final store length: " ++ show (length finalTodos)
  putStrLn "All tests complete!"