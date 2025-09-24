module Parser
    ( parseCommand
) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language (emptyDef)
import Ast

parseCommand :: String -> Either ParseError JToken
parseCommand = parse tokenParser ""

-- | Lexer with reserved keywords
lexer :: TokenParser ()
lexer = makeTokenParser emptyDef
    { reservedNames = ["stampa", "etichetta", "penetra", "catorcino", "è", "pronto", "ciao"]
    , identStart = letter <|> char '_'
    , identLetter = alphaNum <|> char '_'
    }

parser :: Parser JToken
parser = do
    token <- tokenParser
    eof
    return token

-- | Parser for a single token
tokenParser :: Parser JToken
tokenParser =
    choice
        [ try loopParser
        , try printParser
        , try letParser
        , try (Value <$> valueParser)
        , printStackParser
        ]

loopParser :: Parser JToken
loopParser = do
    keywords <- many1 $ reserved lexer "pronto"
    token <- tokenParser
    reserved lexer "ciao"
    let loopSize = length keywords
    if loopSize <= 1
    then return . Cmd $ Loop Infinite token
    else return . Cmd $ Loop (fixedLoop loopSize) token

printParser :: Parser JToken
printParser = do
    reserved lexer "stampa"
    reserved lexer "etichetta"
    Cmd . Print <$> valueParser

printStackParser :: Parser JToken
printStackParser = do
    reserved lexer "penetra"
    return $ Cmd PrintStack

letParser :: Parser JToken
letParser = do
    reserved lexer "catorcino"
    id <- identifier lexer
    reserved lexer "è"
    Cmd . Let id <$> valueParser

valueParser :: Parser JValue
valueParser = choice
    [ try (Int <$> integer lexer)
    , Str <$> stringLiteral lexer
    ]
