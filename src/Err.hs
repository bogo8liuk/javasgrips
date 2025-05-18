module Err
    ( JError(..)
) where

data JError = SyntaxError | RuntimeError

instance Show JError where
    show SyntaxError = "Questa è brutta come frase"
    show RuntimeError = "Ma sei fulminato?"
