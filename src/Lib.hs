module Lib
    ( repl
    ) where

import System.Console.Haskeline
import Control.Monad.State.Lazy (StateT (runStateT), MonadState (get, put), MonadIO (liftIO), MonadTrans (lift))
import Text.Show.Unicode (urecover)
import Ast
import Parser (parseCommand)
import Data.List (foldl')
import Err (JError(..))
import Control.Applicative ((<|>))

-- Nuovo: uno stack di scope
type Scope = [(String, JValue)]
newtype JState = JState { stack :: [Scope] }

type ReplState = StateT JState IO

data EvalResult
    = Val JValue
    | Zero
    | Error JError

-- Helpers

isErr :: EvalResult -> Bool
isErr (Error _) = True
isErr _ = False

getStack :: ReplState [Scope]
getStack = stack <$> get

putStack :: [Scope] -> ReplState ()
putStack st = put $ JState { stack = st }

-- Entry Point

repl :: IO ()
repl = do
    _ <- runStateT (runInputT defaultSettings loop) (JState {stack = [ [] ]})
    return ()
  where
    loop :: InputT ReplState ()
    loop = do
        minput <- getInputLine "AS> "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> do
                evalRes <- lift $ eval input
                eventuallyPrint evalRes
                loop

-- Output

eventuallyPrint :: MonadIO m => EvalResult -> InputT m ()
eventuallyPrint Zero = return ()
eventuallyPrint (Error err) = do
    let showRes = "*** " ++ show err
    liftIO $ putStrLn showRes
eventuallyPrint (Val val) = do
    let showRes = showLess val
    liftIO $ putStrLn showRes

-- Eval input string

eval :: String -> ReplState EvalResult
eval cmd = do
    let parseRes = parseCommand cmd
    case parseRes of
        Left _ -> return $ Error SyntaxError
        Right token -> evalToken token

-- Eval token

evalToken :: JToken -> ReplState EvalResult
evalToken (Value val) = return $ Val val

evalToken (Cmd (Print val)) = do
    liftIO . putStrLn . urecover $ show val
    return Zero

evalToken token@(Cmd (Loop Infinite subToken)) = do
    pushScope
    res <- evalToken subToken
    popScope
    if isErr res
        then return res
        else evalToken token

evalToken (Cmd (Loop (Fixed n) subToken))
    | n <= 0 = return Zero
    | otherwise = evalNTimes n subToken
  where
    evalNTimes 0 _ = return Zero
    evalNTimes i t = do
        pushScope
        res <- evalToken t
        popScope
        if isErr res
            then return res
            else evalNTimes (i - 1) t

evalToken (Cmd (Let id val)) = do
    stored <- declare id val
    if stored
        then return Zero
        else return $ Error RuntimeError

evalToken (Cmd PrintStack) = do
    scopes <- getStack
    let showRes = prettyShowStack scopes
    liftIO . putStrLn $ showRes
    return Zero

-- Scope Management

pushScope :: ReplState ()
pushScope = do
    JState scopes <- get
    put $ JState ([] : scopes)

popScope :: ReplState ()
popScope = do
    JState scopes <- get
    case scopes of
        [] -> error "No scopes to pop"
        (_:rest) -> put $ JState rest

declare :: String -> JValue -> ReplState Bool
declare name val = do
    JState scopes <- get
    case scopes of
        [] -> error "Empty stack (no scopes)"
        (current:rest) ->
            if any (\(n, _) -> n == name) current
                then return False
                else do
                    let newCurrent = (name, val) : current
                    put $ JState (newCurrent : rest)
                    return True

lookupVar :: String -> ReplState (Maybe JValue)
lookupVar name = do
    JState scopes <- get
    return $ foldl (\acc scope -> acc <|> lookup name scope) Nothing scopes

-- Debugging

prettyShowStack :: [Scope] -> String
prettyShowStack scopes =
    unlines $
        zipWith formatScope [0 ..] scopes
  where
    formatScope :: Int -> Scope -> String
    formatScope 0 scope =
        "Global Scope:\n" ++ unlines (map formatBinding scope)
    formatScope i scope =
        "Scope " ++ show i ++ ":\n" ++ unlines (map formatBinding scope)

    formatBinding :: (String, JValue) -> String
    formatBinding (name, val) = "  " ++ name ++ " -> " ++ showLess val
