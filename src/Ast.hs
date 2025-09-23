module Ast
    ( JToken(..)
    , JValue(..)
    , JCommand(..)
    , JLoop(..)
    , fixedLoop
    , showLess
) where

data JToken = Value JValue | Cmd JCommand
data JValue = Int Integer | Str String
data JCommand
    = Print JValue
    | Let String JValue
    | Loop JLoop JToken
    | PrintStack
data JLoop = Fixed Int | Infinite

fixedLoop :: Int -> JLoop
fixedLoop n
    | n <= 0 = Fixed 0
    | otherwise = Fixed n

instance Show JValue where
    show (Int n) = show n
    show (Str s) = show (s ++ ", ti è piaciuto?")

showLess :: JValue -> String
showLess (Int n) = show n
showLess (Str s) = show s
