{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module TemplateHaskell where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Ppr
import Data.List (intercalate)
import Data.Char (toUpper)
import Control.Monad (replicateM, forM, when)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import Debug.Trace (trace)

-------------------------------------------------------------------------------
-- PART 1: TEMPLATE HASKELL BASICS
-------------------------------------------------------------------------------

-- Generate a simple value at compile time
five :: Int
five = $(litE (integerL 5))

-- Generate a list of integers
oneToTen :: [Int]
oneToTen = $(listE (map (litE . integerL) [1..10]))

-- Generate a function that adds its argument to 5
addFive :: Int -> Int
addFive = $(lamE [varP (mkName "x")] (infixE (Just (varE (mkName "x"))) (varE '(Prelude.+)) (Just (litE (integerL 5)))))

-- Generate a simple expression: 2 + 2
twoPlusTwo :: Int
twoPlusTwo = $(infixE (Just (litE (integerL 2))) (varE '(Prelude.+)) (Just (litE (integerL 2))))

-------------------------------------------------------------------------------
-- PART 2: GENERATING DATA TYPES AND INSTANCES
-------------------------------------------------------------------------------

-- Generate a simple data type
data Person = Person { personName :: String, personAge :: Int }
  deriving (Show, Eq)

-- Template Haskell to generate getter functions
makeGetters :: Name -> Q [Dec]
makeGetters typeName = do
  -- Reify the type to get its constructor information
  TyConI (DataD _ _ _ _ [RecC constructorName fields] _) <- reify typeName
  let fieldNames = map (\(fieldName, _, _) -> fieldName) fields
  
  -- Generate getter functions
  getters <- forM fieldNames $ \fieldName -> do
    let getterName = mkName $ "get" ++ nameBase fieldName
    funD getterName [clause [varP (mkName "x")] (normalB (varE 'id `appE` (varE fieldName `appE` varE (mkName "x")))) []]
  
  return getters

-- Generate Show instance for a type
makeShowInstance :: Name -> Q [Dec]
makeShowInstance typeName = do
  TyConI (DataD _ _ _ _ [NormalC constructorName fields] _) <- reify typeName
  let fieldCount = length fields
      fieldVars = map (\i -> mkName $ "x" ++ show i) [1..fieldCount]
      
  -- Create show method
  let showBody = caseE (varE (mkName "x"))
        [ match (conP constructorName (map varP fieldVars))
                (normalB (stringE $ nameBase constructorName ++ " " ++ 
                  intercalate " " (map (\v -> "${show " ++ nameBase v ++ "}") fieldVars))) [] ]
      
  return [InstanceD Nothing [] (AppT (ConT ''Show) (ConT typeName))
            [FunD (mkName "show") [Clause [VarP (mkName "x")] (NormalB showBody) []]]]

-------------------------------------------------------------------------------
-- PART 3: CODE GENERATION PATTERNS
-------------------------------------------------------------------------------

-- Generate record field lenses automatically
makeLenses :: Name -> Q [Dec]
makeLenses typeName = do
  TyConI (DataD _ _ _ _ [RecC constructorName fields] _) <- reify typeName
  let typeConstructor = ConT typeName
      recordTypeName = nameBase typeName
  
  lenses <- forM fields $ \(fieldName, fieldType, _) -> do
    let lensName = mkName $ "_" ++ recordTypeName ++ nameBase fieldName
        getterBody = LamE [VarP (mkName "r")] 
                    (AppE (VarE fieldName) (VarE (mkName "r")))
        setterBody = LamE [VarP (mkName "x"), VarP (mkName "r")] 
                    (RecUpdE (VarE (mkName "r")) [(fieldName, VarE (mkName "x"))])
        
    -- Create lens function: s -> a and s -> a -> s
    return $ ValD (VarP lensName) 
            (NormalB (AppE (AppE (VarE '(\getter setter -> \f s -> f (getter s) (\a -> setter a s))) getterBody) setterBody)) []
  
  return lenses

-- Generate smart constructors with validation
makeSmartConstructor :: Name -> [(String, TypeQ)] -> ExpQ -> Q [Dec]
makeSmartConstructor typeName fields validationExpr = do
  let constructorName = mkName $ "mk" ++ nameBase typeName
      fieldNames = map (\(n, _) -> mkName n) fields
      
  -- Build constructor expression
  let fieldPatterns = map (\(n, _) -> VarP (mkName n)) fields
      fieldExpressions = map (\(n, t) -> (mkName n, VarE (mkName n))) fields
      
  -- Create function with validation
  func <- funD constructorName
    [ clause fieldPatterns
        (normalB $ do
          valid <- validationExpr
          ifE valid 
            (appE (conE typeName) (recConE typeName fieldExpressions))
            (appE (varE 'error) (stringE "Validation failed")))
        [] ]
      
  return func

-------------------------------------------------------------------------------
-- PART 4: DOMAIN-SPECIFIC LANGUAGE GENERATION
-------------------------------------------------------------------------------

-- Generate SQL-like query DSL
data Query = Select [String] String | Where String | OrderBy String
  deriving (Show)

-- Generate query builder functions
makeQueryBuilder :: Q [Dec]
makeQueryBuilder = do
  -- Generate select function
  selectFunc <- funD (mkName "select") 
    [ clause [listP (map (litP . stringL) ["id", "name"])] 
        (normalB (appE (conE 'Select) (listE (map stringE ["id", "name"])))) 
        [] ]
  
  -- Generate from function  
  fromFunc <- funD (mkName "from") 
    [ clause [varP (mkName "query"), litP (stringL "users")]
        (normalB (appE (conE 'Where) (stringE "table = users")))
        [] ]
        
  return $ selectFunc ++ fromFunc

-- Generate HTML DSL
data Html = Tag String [(String, String)] [Html] | Text String
  deriving (Show)

-- Generate HTML element functions
div_ :: [(String, String)] -> [Html] -> Html
div_ attrs children = Tag "div" attrs children

p_ :: String -> Html
p_ text = Tag "p" [] [Text text]

-- Template Haskell to generate HTML element functions
makeHtmlElement :: String -> Q [Dec]
makeHtmlElement tagName = do
  let funcName = mkName $ tagName ++ "_"
      tagNameExpr = stringE tagName
  
  -- Function with attributes and children
  func1 <- funD funcName 
    [ clause [varP (mkName "attrs"), varP (mkName "children")]
        (normalB (appE (appE (appE (conE 'Tag) tagNameExpr) (varE (mkName "attrs"))) (varE (mkName "children"))))
        [] ]
  
  -- Function with only text content
  func2 <- funD funcName 
    [ clause [varP (mkName "text")]
        (normalB (appE (appE (appE (conE 'Tag) tagNameExpr) (listE [])) (listE [appE (conE 'Text) (varE (mkName "text"))])))
        [] ]
        
  return $ func1 ++ func2

-------------------------------------------------------------------------------
-- PART 5: COMPILE-TIME COMPUTATION AND CODE OPTIMIZATION
-------------------------------------------------------------------------------

-- Generate pre-computed values at compile time
fibonacciTable :: [Integer]
fibonacciTable = $(do
  let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
  listE (map (litE . integerL) (take 100 fibs)))

-- Generate optimized pattern matching
makeOptimizedMatcher :: [(String, ExpQ)] -> Q [Dec]
makeOptimizedMatcher patterns = do
  let matcherName = mkName "optimizedMatch"
      
  cases <- forM (zip [0..] patterns) $ \(i, (pattern, result)) -> do
    match (litP (stringL pattern)) (normalB result) []
    
  func <- funD matcherName [clause [varP (mkName "x")] (normalB (caseE (varE (mkName "x")) cases)) []]
  return [func]

-- Generate lookup table for trigonometric functions
trigTable :: [(Double, Double, Double)]
trigTable = $(do
  let angles = [0, pi/6, pi/4, pi/3, pi/2]
      entries = map (\angle -> (angle, sin angle, cos angle)) angles
  listE (map (\(a, s, c) -> tupE [litE (rationalL (toRational a)), 
                                   litE (rationalL (toRational s)), 
                                   litE (rationalL (toRational c))]) entries))

-------------------------------------------------------------------------------
-- PART 6: TYPE-DRIVEN CODE GENERATION
-------------------------------------------------------------------------------

-- Generate functions based on type information
generateArithOps :: Name -> Q [Dec]
generateArithOps typeName = do
  TyConI (DataD _ _ _ _ _ _) <- reify typeName
  
  let addName = mkName $ "add" ++ nameBase typeName
      mulName = mkName $ "mul" ++ nameBase typeName
      showName = mkName $ "show" ++ nameBase typeName
  
  -- Check if type is numeric
  isNum <- isInstance ''Num [VarT (mkName "a")]
  
  if isNum
    then do
      addFunc <- funD addName
        [ clause [varP (mkName "x"), varP (mkName "y")]
            (normalB (infixE (Just (varE (mkName "x"))) (varE '(Prelude.+)) (Just (varE (mkName "y")))))
            [] ]
            
      mulFunc <- funD mulName
        [ clause [varP (mkName "x"), varP (mkName "y")]
            (normalB (infixE (Just (varE (mkName "x"))) (varE '(Prelude.*)) (Just (varE (mkName "y")))))
            [] ]
            
      return $ addFunc ++ mulFunc
    else return []

-- Generate FromJSON/ToJSON instances automatically
makeJsonInstances :: Name -> Q [Dec]
makeJsonInstances typeName = do
  TyConI (DataD _ _ _ _ [RecC constructorName fields] _) <- reify typeName
  
  let fieldNames = map (\(fieldName, _, _) -> nameBase fieldName) fields
      fieldTypes = map (\(_, fieldType, _) -> fieldType) fields
      
  -- Generate ToJSON instance
  toJsonFunc <- [d|
    instance ToJSON $(conT typeName) where
      toJSON $(conP constructorName (map (\n -> varP (mkName n)) fieldNames)) = 
        object $(listE (map (\n -> [| $(stringE n) .= $(varE (mkName n)) |]) fieldNames))
    |]
    
  -- Generate FromJSON instance  
  fromJsonFunc <- [d|
    instance FromJSON $(conT typeName) where
      parseJSON = withObject $(stringE (nameBase typeName)) $ \obj -> 
        $(conE constructorName) 
          <$> $(foldl (\acc n -> [| $acc <*> obj .: $(stringE n) |]) [| pure () |] fieldNames)
    |]
    
  return $ toJsonFunc ++ fromJsonFunc

-------------------------------------------------------------------------------
-- PART 7: QUOTATIONS AND QUASI-QUOTERS
-------------------------------------------------------------------------------

-- Custom quasiquoter for SQL
sql :: QuasiQuoter
sql = QuasiQuoter
  { quoteExp = \str -> [| Query (parseSql str) |]
  , quotePat = error "sql quoter not supported in patterns"
  , quoteType = error "sql quoter not supported in types"
  , quoteDec = error "sql quoter not supported in declarations"
  }
  where
    parseSql = id  -- Simplified parser

-- Example SQL query using quasiquoter
exampleQuery :: Query
exampleQuery = [sql| SELECT id, name FROM users WHERE age > 18 |]

-- Custom quasiquoter for regular expressions
regex :: QuasiQuoter
regex = QuasiQuoter
  { quoteExp = \pattern -> [| compileRegex pattern |]
  , quotePat = \pattern -> do
      pat <- compileRegex pattern
      return $ ConP 'Matches [LitP (StringL pattern), VarP (mkName "str")]
  , quoteType = error "regex quoter not supported in types"
  , quoteDec = error "regex quoter not supported in declarations"
  }
  where
    compileRegex = stringE

data Matches = Matches String String

-- Example regex using quasiquoter
exampleRegex :: Matches
exampleRegex = [regex|^[a-zA-Z]+$|]

-------------------------------------------------------------------------------
-- PART 8: MACROS AND METAPROGRAMMING UTILITIES
-------------------------------------------------------------------------------

-- Logging macro that includes source location
logInfo :: String -> Q Exp
logInfo message = do
  loc <- location
  let file = loc_filename loc
      line = fst (loc_start loc)
      col = snd (loc_start loc)
      
  [| putStrLn ($(stringE file) ++ ":" ++ show $(litE (integerL line)) ++ 
               ":" ++ show $(litE (integerL col)) ++ " - " ++ $(stringE message)) |]

-- Trace macro for debugging
traceExpr :: ExpQ -> ExpQ
traceExpr expr = do
  exprStr <- pprint <$> expr
  [| trace $(stringE exprStr) $(expr) |]

-- Generate enumeration type with values
makeEnum :: String -> [String] -> Q [Dec]
makeEnum typeName values = do
  let constrs = map (\v -> NormalC (mkName v) []) values
      enumType = DataD [] (mkName typeName) [] Nothing constrs [] []
      
  -- Generate Show instance
  showInstance <- [d|
    instance Show $(conT (mkName typeName)) where
      show $(conP (mkName (head values)) []) = $(stringE (head values))
      $(generateShowMatches (tail values))
    |]
    
  -- Generate fromString/toString functions
  toStringFunc <- funD (mkName $ "toString" ++ typeName)
    [ clause [conP (mkName v) []]
        (normalB (stringE v)) [] | v <- values ]
        
  fromStringFunc <- funD (mkName $ "fromString" ++ typeName)
    [ clause [litP (stringL v)]
        (normalB (conE (mkName v))) [] | v <- values ]
    
  return $ [enumType] ++ showInstance ++ toStringFunc ++ fromStringFunc

generateShowMatches :: [String] -> [MatchQ]
generateShowMatches [] = []
generateShowMatches (v:vs) = 
  [ match (conP (mkName v) [])
      (normalB (stringE v)) [] ] ++ generateShowMatches vs

-------------------------------------------------------------------------------
-- PART 9: COMPILE-TIME VALIDATION AND CODE ANALYSIS
-------------------------------------------------------------------------------

-- Validate that a type has certain instances
validateInstances :: [Name] -> Name -> Q [Dec]
validateInstances requiredInstances typeName = do
  missing <- filterM (\inst -> not <$> isInstance inst [ConT typeName]) requiredInstances
  
  when (not (null missing)) $ do
    reportError $ "Type " ++ nameBase typeName ++ " missing instances: " ++ 
                  intercalate ", " (map nameBase missing)
    
  return []

-- Check that all record fields start with lowercase
validateRecordFields :: Name -> Q [Dec]
validateRecordFields typeName = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify typeName
  
  let invalidFields = filter (\(fieldName, _, _) -> 
        case nameBase fieldName of
          c:_ -> c `elem` ['A'..'Z']
          _ -> False) fields
      
  when (not (null invalidFields)) $ do
    reportError $ "Type " ++ nameBase typeName ++ 
                 " has capitalized record fields: " ++
                 intercalate ", " (map (nameBase . (\(n,_,_) -> n)) invalidFields)
                 
  return []

-- Generate size information for types
typeSize :: Name -> Q Exp
typeSize typeName = do
  TyConI (DataD _ _ _ _ constructors _) <- reify typeName
  
  let constructorCount = length constructors
      fieldCounts = map (\case
        NormalC _ fields -> length fields
        RecC _ fields -> length fields
        _ -> 0) constructors
      totalFields = sum fieldCounts
      
  [| putStrLn $(stringE $ "Type " ++ nameBase typeName ++ 
                        " has " ++ show constructorCount ++ " constructors and " ++ 
                        show totalFields ++ " total fields") |]

-------------------------------------------------------------------------------
-- PART 10: REAL-WORLD TEMPLATE HASKELL APPLICATIONS
-------------------------------------------------------------------------------

-- Example 1: Generate command-line parser
data Command = CmdRun | CmdVersion | CmdHelp
  deriving (Show)

makeCommandParser :: Q [Dec]
makeCommandParser = do
  -- Generate parser for each command
  let commands = [("run", CmdRun), ("version", CmdVersion), ("help", CmdHelp)]
      
  parser <- funD (mkName "parseCommand")
    [ clause [litP (stringL cmd)]
        (normalB (conE constr)) [] | (cmd, constr) <- commands ]
    
  -- Generate help text
  helpText <- funD (mkName "commandHelp")
    [ clause [] 
        (normalB (stringE $ "Available commands: " ++ intercalate ", " (map fst commands))) 
        [] ]
        
  return $ parser ++ helpText

-- Example 2: Generate REST API endpoints
data Endpoint = Get String | Post String | Put String | Delete String
  deriving (Show)

makeEndpoint :: String -> String -> Q Exp
makeEndpoint method path = do
  let endpoint = case method of
        "GET" -> conE 'Get
        "POST" -> conE 'Post  
        "PUT" -> conE 'Put
        "DELETE" -> conE 'Delete
        _ -> error $ "Unknown method: " ++ method
      
  [| $endpoint $(stringE path) |]

-- Example 3: Generate test cases
makePropertyTests :: Name -> Q [Dec]
makePropertyTests typeName = do
  TyConI (DataD _ _ _ _ constructors _) <- reify typeName
  
  tests <- forM constructors $ \case
    NormalC constrName fields -> do
      let testName = mkName $ "test_" ++ nameBase constrName
          fieldCount = length fields
          
      -- Generate test that creates value and checks it
      return $ ValD (VarP testName)
        (NormalB [| \x -> $(conE constrName) x == $(conE constrName) x |]) []
        
    _ -> return []
    
  return (concat tests)

-------------------------------------------------------------------------------
-- MAIN DEMONSTRATION
-------------------------------------------------------------------------------

$(makeGetters ''Person)

-- Generate enum type
$(makeEnum "Color" ["Red", "Green", "Blue"])

-- Generate JSON instances
{-
$(makeJsonInstances ''Person)
-}

-- Generate HTML elements
$(makeHtmlElement "div")
$(makeHtmlElement "span")
$(makeHtmlElement "button")

-- Generate command parser
$(makeCommandParser)

main :: IO ()
main = do
  putStrLn "=== TEMPLATE HASKELL DEMONSTRATION ==="
  putStrLn ""
  
  putStrLn "1. Generated Values:"
  putStrLn $ "   five = " ++ show five
  putStrLn $ "   oneToTen = " ++ show (take 5 oneToTen) ++ "..."
  putStrLn $ "   addFive 10 = " ++ show (addFive 10)
  putStrLn $ "   2 + 2 = " ++ show twoPlusTwo
  
  putStrLn "\n2. Fibonacci Table (first 10):"
  putStrLn $ "   " ++ show (take 10 fibonacciTable)
  
  putStrLn "\n3. Trigonometric Table:"
  putStrLn $ "   " ++ show trigTable
  
  putStrLn "\n4. Generated HTML DSL:"
  let html = div_ [("class", "container")] [p_ "Hello, world!"]
  putStrLn $ "   " ++ show html
  
  putStrLn "\n5. Generated Enum Type:"
  putStrLn $ "   Colors: " ++ show [Red, Green, Blue]
  
  putStrLn "\n6. Command Parser:"
  putStrLn $ "   parseCommand \"run\" = " ++ show (parseCommand "run")
  putStrLn $ "   commandHelp: " ++ commandHelp
  
  putStrLn "\n7. Record Field Getters (generated for Person):"
  let person = Person "Alice" 30
  putStrLn $ "   getName person = " ++ getpersonName person
  putStrLn $ "   getAge person = " ++ show (getpersonAge person)
  
  putStrLn "\n=== TEMPLATE HASKELL USE CASES ==="
  putStrLn """
  Common Applications:
  1. Boilerplate Reduction: Generate instances (Show, Eq, Ord, JSON, etc.)
  2. DSL Implementation: Embed domain-specific languages
  3. Code Generation: Generate functions from specifications
  4. Compile-time Validation: Check program properties before runtime
  5. Optimization: Generate specialized, optimized code
  6. Metaprogramming: Write programs that write programs
  
  When to Use Template Haskell:
  • Repetitive boilerplate code
  • DSL implementation
  • Code generation from external specifications
  • Compile-time computation and optimization
  • Type-safe metaprogramming
  
  When to Avoid Template Haskell:
  • Simple problems with manual solutions
  • Code that needs to be easily debuggable
  • Projects with strict compile-time requirements
  • Beginners unfamiliar with Haskell's type system
  
  Best Practices:
  1. Keep Template Haskell code simple and readable
  2. Provide escape hatches (manual implementations)
  3. Document generated code thoroughly
  4. Test Template Haskell separately from regular code
  5. Consider code size impact on compilation
  
  Advanced Patterns:
  1. Type-level metaprogramming
  2. Quasi-quoters for embedded DSLs
  3. Compile-time code analysis
  4. Plugin architecture
  5. Staged computation
  
  Popular Libraries Using Template Haskell:
  • lens: Optical getters/setters
  • aeson: JSON serialization
  • persistent: Database mappings
  • yesod: Web framework routing
  • servant: API type-safe clients
  """
  
  putStrLn "\n=== COMPILATION NOTES ==="
  putStrLn """
  Template Haskell requires:
  1. {-# LANGUAGE TemplateHaskell #-} pragma
  2. TemplateHaskell package installed
  3. Stage restriction: Template Haskell runs at compile time
  
  Common Compilation Issues:
  1. Stage restriction errors: Use -fplugin to work around
  2. Code size explosion: Generated code can be large
  3. Compile time increase: TH adds overhead
  4. Cross-module dependencies: TH sees only fully compiled modules
  
  Debugging Template Haskell:
  1. Use $(pprint ...) to inspect generated code
  2. Enable -ddump-splices to see spliced code
  3. Use reportError for compile-time error messages
  4. Test TH code in isolation
  """
  
  putStrLn "\nRunning all examples..."
  runAllExamples

runAllExamples :: IO ()
runAllExamples = do
  putStrLn "\n=== RUNNING GENERATED CODE ==="
  
  -- Test generated values
  putStrLn "Testing generated functions:"
  putStrLn $ "   fibonacciTable[5] = " ++ show (fibonacciTable !! 5)
  putStrLn $ "   Add function: 5 + 10 = " ++ show (5 + 10)
  
  -- Test HTML DSL
  putStrLn "\nGenerated HTML:"
  let page = div_ [] [
        div_ [("class", "header")] [p_ "Welcome"],
        div_ [("class", "content")] [p_ "Content here"],
        div_ [("class", "footer")] [p_ "Goodbye"]
      ]
  putStrLn $ "   " ++ show page
  
  -- Test command parsing
  putStrLn "\nCommand parsing:"
  putStrLn $ "   'run' -> " ++ show (parseCommand "run")
  putStrLn $ "   'help' -> " ++ show (parseCommand "help")
  
  putStrLn "\nAll Template Haskell examples executed successfully!"

-- Helper functions for demonstration
parseCommand :: String -> Command
parseCommand "run" = CmdRun
parseCommand "version" = CmdVersion
parseCommand "help" = CmdHelp
parseCommand _ = error "Unknown command"

getpersonName :: Person -> String
getpersonName (Person name _) = name

getpersonAge :: Person -> Int
getpersonAge (Person _ age) = age

-- Compile-time logging example
debugExample :: IO ()
debugExample = $(logInfo "Template Haskell example executing")

-- Type size information
typeInfo :: IO ()
typeInfo = $(typeSize ''Person)