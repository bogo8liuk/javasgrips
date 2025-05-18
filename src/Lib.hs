module Lib
    ( repl
) where

import System.Console.Haskeline
import Control.Monad.State.Lazy (StateT (runStateT), MonadState (get, put), MonadIO (liftIO), MonadTrans (lift))
import Text.Show.Unicode (urecover)
import Ast
import Parser (parseCommand)
import Data.List (foldl')

newtype JState = JState { stack :: [(String, JValue)] }

type ReplState = StateT JState IO

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
                    resToPrint <- lift $ eval input
                    eventuallyPrint resToPrint
                    loop

eventuallyPrint :: MonadIO m => Maybe String -> InputT m ()
eventuallyPrint Nothing = return ()
eventuallyPrint (Just s) = liftIO . putStrLn $ urecover s

eval :: String -> ReplState (Maybe String)
eval cmd = do
    let parseRes = parseCommand cmd
    case parseRes of
        Left err -> return Nothing
        Right token -> evalToken token

evalToken :: JToken -> ReplState (Maybe String)
evalToken (Value val) = return . Just $ showLess val
evalToken (Cmd (Print val)) = return . Just $ show val
evalToken (Cmd (Let id val)) = do
    store id val
    return Nothing
evalToken (Cmd PrintStack) = do
    stack <- getStack
    return . Just $ prettyShowStack stack

store :: String -> JValue -> ReplState ()
store id x = do
    state <- get
    put $ JState {stack = (id, x) : stack state}
    return ()

prettyShowStack :: [(String, JValue)] -> String
prettyShowStack bindings = foldl' concat "" bindings
    where
        concat cur (id, val) = cur ++ withPad id ++ "-> " ++ showLess val ++ "\n"

        withPad str = str ++ map (const ' ') [1..(padding - length str)]

        padding = foldl' (\curMax (id, _) -> max curMax $ length id) 0 bindings + 1
