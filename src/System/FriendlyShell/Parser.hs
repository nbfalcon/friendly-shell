{-# LANGUAGE OverloadedStrings #-}

module System.FriendlyShell.Parser (parseModule, parseStatement) where

import Control.Applicative hiding (some, many)
import Data.List.NonEmpty hiding (singleton)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import System.FriendlyShell.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Combinators
type Parser = Parsec Void Text

some1L :: Parser a -> Parser [a]
some1L = (toList <$>) . some1

some1T :: Parser Char -> Parser Text
some1T = (T.pack <$>) . some1L

-- Fundamental lexical stuff
spaceLit :: Parser ()
spaceLit = L.space hspace1 (L.skipLineComment "#" <|> L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceLit

symbol :: Text -> Parser Text
symbol = L.symbol spaceLit

-- Basic tokens
stringLit :: Parser Text
stringLit =
    fmap T.pack $
        (char '"' >> manyTill L.charLiteral (char '"'))
            <|> (char '\'' >> manyTill L.charLiteral (char '\''))

identifierAtom :: Parser Text
identifierAtom = some1T $ alphaNumChar <|> char '_' <|> char '-' <|> char '/'

varAtom :: Parser String
varAtom = (char '$' >>) $ some1L $ alphaNumChar <|> char '_'

-- Grammar
execCmd :: Parser ExecuteCommand
execCmd = ExecuteCommand <$> lexeme atom <*> many (lexeme atom) <*> optional (symbol "|" >> execCmd)

executeForStdoutExpr :: Parser AComponent
executeForStdoutExpr = between (symbol "(") (symbol ")") $ CExecuteSubcommandForStdout <$> execCmd

component :: Parser AComponent
component =
    CConstant <$> identifierAtom
        <|> CConstant <$> stringLit
        <|> CVarRef <$> varAtom
        <|> executeForStdoutExpr

atom :: Parser SAtom
atom = AString <$> some1L component

parseAssignStmt :: Parser SStatement
parseAssignStmt = SAssignVar <$> lexeme varAtom <* lexeme (char '=') <*> lexeme atom

parseExecStmt :: Parser SStatement
parseExecStmt = SExecuteShell <$> execCmd

parseStatement :: Parser SStatement
parseStatement = (spaceLit >>) $ try parseAssignStmt <|> parseExecStmt

parseModule :: Parser SModule
parseModule = SModule <$> many parseStatement