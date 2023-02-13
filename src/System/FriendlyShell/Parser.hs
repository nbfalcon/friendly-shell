{-# LANGUAGE OverloadedStrings #-}

module System.FriendlyShell.Parser (parseModule, parseStatement) where

import Control.Applicative hiding (many, some)
import Control.Monad.Combinators.Expr
import Data.List.NonEmpty hiding (singleton)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import System.FriendlyShell.AST
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf qualified as T

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

stringLitP' :: Char -> Parser Text
stringLitP' c = char c >> T.pack <$> manyTill L.charLiteral (char $ T.fromChar c)

-- Basic tokens
stringLit :: Parser Text
stringLit = stringLitP' '\'' <|> stringLitP' '"'

identifierAtom :: Parser Text
identifierAtom = some1T $ alphaNumChar <|> char '_' <|> char '-' <|> char '/'

varIdentifier :: Parser String
varIdentifier = some1L $ alphaNumChar <|> char '_'

varAtom :: Parser String
varAtom = char '$' >> varIdentifier

-- Grammar
execCmd :: Parser ExecuteCommand
execCmd = ExecuteCommand <$> lexeme atom <*> many (lexeme atom) <*> optional (symbol "|" >> execCmd)

executeForStdoutExpr' :: Parser ExecuteCommand
executeForStdoutExpr' = (char '$' >>) $ between (symbol "(") (symbol ")") execCmd

executeForStdoutExpr :: Parser AComponent
executeForStdoutExpr = CExecuteSubcommandForStdout <$> executeForStdoutExpr'

arithLikeExpr :: Parser ArithExpr
arithLikeExpr
    = FVarRef <$> varIdentifier
    <|> FForStdout <$> executeForStdoutExpr'
    <|> makeExprParser
        arithLikeExpr
        [ [prefix "+" id, prefix "-" FUnegExpr]
        ,
            [ binary "+" FAddExpr
            , binary "-" FSubExpr
            , binary "*" FMulExpr
            , binary "/" FDivExpr
            , binaryR ".." FCatExpr
            ]
        ]
prefix c f = Prefix (f <$ symbol c)
binary c f = InfixL (f <$ symbol c)
binaryR c f = InfixR (f <$ symbol c)

arithExpr :: Parser AComponent
arithExpr = fmap CArithExpr $ symbol "${" *> arithLikeExpr <* char '}'

component :: Parser AComponent
component =
    CConstant <$> identifierAtom
        <|> CConstant <$> stringLit
        <|> CVarRef <$> try varAtom
        <|> arithExpr
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