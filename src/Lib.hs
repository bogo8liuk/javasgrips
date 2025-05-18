module Lib
    ( repl
) where

import System.Console.Haskeline
import Control.Monad.State.Lazy (StateT (runStateT), MonadState (get, put), MonadIO (liftIO), MonadTrans (lift))
import Text.Show.Unicode (urecover)
import Ast
import Parser (parseCommand)

newtype JState = JState { stack :: [JValue] }

type ReplState = StateT JState IO

getStack :: ReplState [JValue]
getStack = stack <$> get

putStack :: [JValue] -> ReplState ()
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
evalToken (Value item) = do
    store item
    return Nothing
evalToken (Cmd (Print val)) = return . Just $ show val

store :: JValue -> ReplState ()
store x = do
    state <- get
    put $ JState {stack = x : stack state}
    return ()
