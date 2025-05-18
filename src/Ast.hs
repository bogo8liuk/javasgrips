module Ast
    ( JToken(..)
    , JValue(..)
    , JCommand(..)
    , showLess
) where

data JToken = Value JValue | Cmd JCommand
data JValue = Int Integer | Str String
data JCommand = Print JValue | Let String JValue | PrintStack

instance Show JValue where
    show (Int n) = show n
    show (Str s) = show (s ++ ", ti è piaciuto?")

showLess :: JValue -> String
showLess (Int n) = show n
showLess (Str s) = show s
