module Parser.SMTLibParser where

import Text.Parsec.Language (GenLanguageDef)
import Text.Parsec.Token (GenLanguageDef(..), makeTokenParser, GenTokenParser (stringLiteral, identifier, parens))
import Data.Functor.Identity (Identity)
import Text.Parsec (alphaNum, oneOf, (<|>), Parsec, many, eof, newline, parse)

type Parser = Parsec String ()

languageDef :: GenLanguageDef String () Identity
languageDef = LanguageDef {
        commentStart = "",
        commentEnd = "",
        commentLine = ";",
        nestedComments = False,
        identStart = alphaNum <|> oneOf "-_.+*:<>=",
        identLetter = alphaNum <|> oneOf "-_.+*:<>=",
        opStart = oneOf [],
        opLetter = oneOf [],
        reservedNames = [],
        reservedOpNames = [],
        caseSensitive = True
    }

lexer :: GenTokenParser String () Identity
lexer = makeTokenParser languageDef

data Expression = Parenthesized [Expression]
                | Atom String
                | StringLiteral String
                deriving (Eq, Show)

expression :: Parser Expression
expression = parenthesized <|> atom <|> stringLit

atom :: Parser Expression
atom = Atom <$> identifier lexer

stringLit :: Parser Expression
stringLit = StringLiteral <$> stringLiteral lexer

parenthesized :: Parser Expression
parenthesized = Parenthesized <$> (parens lexer (many expression))

smtFile :: Parser [Expression]
smtFile = many (expression <* many newline) <* eof

parseSMTFile :: FilePath -> IO [Expression]
parseSMTFile path = do
    contents <- readFile path
    let result = parse smtFile path contents
    case result of
        Left err -> error (show err)
        Right expressions -> return expressions
