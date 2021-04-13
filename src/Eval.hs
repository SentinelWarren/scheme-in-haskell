module Eval ( eval, bindVars, apply ) where
import Datatypes
import Parser
import Data.IORef
import Data.Functor ( (<&>) )
import Data.Maybe (isJust, isNothing)

eval :: Env -> LispVal -> IOThrowsError LispVal

-----------------------------------
-- ********** Primitives **********
-----------------------------------

eval _ val@(Float _) = return val
eval _ val@(Number _) = return val
eval _ val@(String _) = return val
eval _ val@(Bool _) = return val
eval _ (List [Atom "quote", val]) = return val

---------------------------------------
-- ***** State-related Evaluators *****
---------------------------------------

eval env (Atom id) = getVar env id
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var

----------------------------------
-- ********** if & cond **********
----------------------------------

eval env (List [Atom "if", pred, conseq, alt]) = do
  result <- eval env pred
  case result of
    Bool False -> eval env alt
    _          -> eval env conseq
eval env (List (Atom "cond" : pairs)) = evalCond pairs
  where evalCond (List [Atom "else", value] : []) = eval env value
        evalCond (List [condition, value] : rest) = do
          conditionResult <- eval env condition
          case conditionResult of
              Bool False -> evalCond rest
              _ -> eval env value
        evalCond [] = pure $ Atom ""

----------------------------------
-- ********** Functions **********
----------------------------------

eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body

-------------------------------------------
-- ***** Catch-all user-defined funcs *****
-------------------------------------------

eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

----------------------------------
-- ********** Bad forms **********
----------------------------------

eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

--------------------------------------------
-- ********** State-related Funcs **********
--------------------------------------------

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> (Data.Maybe.isJust . lookup var)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . flip writeIORef value)
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv
  bindings >>= newIORef
  where extendEnv bindings env = fmap (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
          ref <- newIORef value
          return (var, ref)

--------------------------------
-- ********** Helpers **********
--------------------------------

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
      then throwError  $ NumArgs (num params) args
      else liftIO (bindVars closure $ zip params args) >>=
        bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = last <$> mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal