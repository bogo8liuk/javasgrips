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

newtype JState = JState { stack :: [(String, JValue)] }

type ReplState = StateT JState IO

data EvalResult
    = Val JValue
    | Zero
    | Error JError

isErr :: EvalResult -> Bool
isErr (Error _) = True
isErr _ = False

getStack :: ReplState [(String, JValue)]
getStack = stack <$> get

putStack :: [(String, JValue)] -> ReplState ()
putStack st = put $ JState { stack = st }

repl :: IO ()
repl = do
    _ <- runStateT (runInputT defaultSettings loop) (JState {stack = []})
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

eventuallyPrint :: MonadIO m => EvalResult -> InputT m ()
eventuallyPrint Zero = return ()
eventuallyPrint (Error err) = do
    let showRes = "*** " ++ show err
    liftIO $ putStrLn showRes
eventuallyPrint (Val val) = do
    let showRes = showLess val
    liftIO $ putStrLn showRes

eval :: String -> ReplState EvalResult
eval cmd = do
    let parseRes = parseCommand cmd
    case parseRes of
        Left _ -> return $ Error SyntaxError
        Right token -> evalToken token

evalToken :: JToken -> ReplState EvalResult
evalToken (Value val) = return $ Val val
evalToken (Cmd (Print val)) = do
    liftIO . putStrLn . urecover $ show val
    return Zero
evalToken token@(Cmd (Loop Infinite subToken)) = do
    res <- evalToken subToken
    if isErr res
    then return res
    else evalToken token
evalToken (Cmd (Loop (Fixed n) subToken))
    | n <= 0 = return Zero
    | otherwise = do
        res <- evalToken subToken
        if isErr res
        then return res
        else evalToken . Cmd $ Loop (Fixed (n - 1)) subToken
evalToken (Cmd (Let id val)) = do
    stored <- store id val
    if stored
    then return Zero
    else return $ Error RuntimeError
evalToken (Cmd PrintStack) = do
    stack <- getStack
    let showRes = prettyShowStack stack
    liftIO . putStrLn $ showRes
    return Zero

store :: String -> JValue -> ReplState Bool
store id x = do
    state <- get
    if any (\(id', _) -> id == id') $ stack state
    then return False
    else do
        put $ JState {stack = (id, x) : stack state}
        return True

prettyShowStack :: [(String, JValue)] -> String
prettyShowStack bindings = foldl' concat "" bindings
    where
        concat cur (id, val) = cur ++ withPad id ++ "-> " ++ showLess val ++ "\n"

        withPad str = str ++ map (const ' ') [1..(padding - length str)]

        padding = foldl' (\curMax (id, _) -> max curMax $ length id) 0 bindings + 1
