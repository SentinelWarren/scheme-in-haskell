module Datatypes ( LispVal(..), LispError(..), ThrowsError, IOThrowsError, Env, showVal, nullEnv, throwError, catchError, trapError, extractValue, liftThrows, liftIO ) where
import Control.Monad.Except
import Text.ParserCombinators.Parsec(ParseError)
import Data.IORef
import Data.Maybe

-----------------------------------
-- ********** Data Types **********
-----------------------------------

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: Maybe String,
                     body :: [LispVal], closure :: Env}

data LispError = NumArgs Integer [LispVal]
               | FloArgs Float [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

------------------------------------
-- ********** Environment **********
------------------------------------

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

-----------------------------------------
-- ********** Display LispVals **********
-----------------------------------------

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Float contents) = show contents
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} =
  "(lambda (" ++ unwords (map show args) ++
      (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where show = showVal

---------------------------------------
-- ********** Display Errors **********
---------------------------------------

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++
  " args: found values " ++ unwordsList found
showError (FloArgs expected found) = "Expected " ++ show expected ++
  " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++
  expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-------------------------------------
-- ********** Helper Funcs **********
-------------------------------------

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-------------------------------------------------
-- ********** Utility Funcs (exported) **********
-------------------------------------------------

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
