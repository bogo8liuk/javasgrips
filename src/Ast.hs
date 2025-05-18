module Ast
    ( JToken(..)
    , JValue(..)
    , JCommand(..)
) where

data JToken = Value JValue | Cmd JCommand
data JValue = Int Integer | Str String
data JCommand = Print JValue

instance Show JValue where
    show (Int n) = show n
    show (Str s) = show (s ++ ", ti è piaciuto?")

