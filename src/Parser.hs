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
    { reservedNames = ["stampa", "etichetta", "penetra", "catorcino", "è"]
    , identStart = letter <|> char '_'
    , identLetter = alphaNum <|> char '_' 
    }

-- | Parser for a single token
tokenParser :: Parser JToken
tokenParser = do
    token <- choice
        [ try printParser
        , try letParser
        , try (Value <$> valueParser)
        , printStackParser
        ]
    eof
    return token

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
