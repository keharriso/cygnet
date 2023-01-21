{-# LANGUAGE ImportQualifiedPost #-}

module Cygnet.Parser (parseCygnet) where

import Control.Monad (void)
import Data.Map qualified as Map
import Text.Parsec
import Text.Parsec.Indent

import Cygnet.AST

type CygnetParser a = IndentParser String Module a

parseCygnet :: String -> String -> String -> Either ParseError Module
parseCygnet source name text =
    let state = Module{moduleName = name, moduleIncludes = [], moduleSymbols = Map.empty}
     in runIndent $ runParserT parseModule state source text

updateUserState :: (Module -> Module) -> CygnetParser Module
updateUserState update = do
    parserState <- updateParserState (\state -> state{stateUser = update (stateUser state)})
    return $ stateUser parserState

getUserState :: CygnetParser Module
getUserState = stateUser <$> getParserState

parseModule :: CygnetParser Module
parseModule = do
    header
    definitions
    eof
    getUserState

header :: CygnetParser ()
header = do
    includes <- next >> endBy include next
    void $ updateUserState (\state -> state{moduleIncludes = includes})

include :: CygnetParser String
include = topLevel >> withPos (string "include" *> spaces *> same *> stringLiteral <* manyTill space endl)

definitions :: CygnetParser ()
definitions = do
    symbols <- next >> endBy parseTopLevel next
    void $ updateUserState (\state -> state{moduleSymbols = Map.fromList [(name, symbol) | symbol@(Symbol _ _ name _) <- symbols]})

parseTopLevel :: CygnetParser Symbol
parseTopLevel =
    topLevel
        >> withPos
            ( do
                access <- parseAccess
                next
                linkage <- parseLinkage
                next
                name <- parseSymbolName
                same
                next
                sameOrIndented
                void $ char ':'
                next
                fnType <- parseType
                next
                (fnParams, fnBody) <- parseBody name
                return $ Symbol access linkage name (Function fnBody fnType fnParams)
            )

parseAccess :: CygnetParser Access
parseAccess = (try (string "export" >> space) >> return Public) <|> return Private

parseLinkage :: CygnetParser Linkage
parseLinkage = (try (string "foreign" >> space) >> return C) <|> return Cygnet

parseSymbolName :: CygnetParser String
parseSymbolName = many1 $ alphaNum <|> oneOf "_-"

parseTypeName :: CygnetParser String
parseTypeName = parseSymbolName

parseType :: CygnetParser Type
parseType = do
    curriedTypes <- sepBy1 (sameOrIndented >> parseAtomicType <* next) (sameOrIndented >> string "->" >> next)
    return $ foldr1 TFunction $ if length curriedTypes == 1 then TVoid : curriedTypes else curriedTypes
  where
    parseAtomicType =
        try (string "void" >> return TVoid)
            <|> try (string "string" >> return TString)
            <|> try (string "int" >> return TInt)

parseBody :: String -> CygnetParser ([String], Block)
parseBody name =
    topLevel
        >> withPos
            ( do
                string name >> sepAhead >> next
                params <- parseParams
                body <- sameOrIndented >> char '=' >> sepAhead >> next >> sameOrIndented >> parseDefinition
                return (if null params then [""] else params, body)
            )
  where
    parseParams = sepBy (sameOrIndented >> parseSymbolName <* sepAhead) next
    parseDefinition = parseBlock "do" <|> ((: []) <$> parseStatement <* next)
    parseStatement = parseReturn <|> (SExpression <$> parseExpression)
    parseBlock kw =
        let parseBlockBegin = try (string kw >> sepAhead) >> next >> parseBlockBody
            parseBlockBody = sameOrIndented >> withPos (sepBy1 (checkIndent >> parseStatement) next)
         in (same >> parseBlockBegin) <|> withPos parseBlockBegin
    parseReturn = withPos (try (string "return" >> sepAhead) >> next >> sameOrIndented >> (SReturn <$> parseExpression))
    parseExpression = parseLiteral <|> parseApply
    parseLiteral = ((ELiteral . LString <$> stringLiteral) <|> numberLiteral) <* next
    parseArg = parseLiteral <|> parseNamed <|> parseParenExpr
    parseApply = withPos (EApply <$> sepBy1 (sameOrIndented >> parseArg) next)
    parseNamed = ENamed <$> parseSymbolName <* sepAhead
    parseParenExpr = char '(' >> next >> (EApply <$> sepBy1 parseArg next) <* next <* char ')'

stringLiteral :: CygnetParser String
stringLiteral = char '"' *> many quotedChar <* char '"'

quotedChar :: CygnetParser Char
quotedChar = noneOf "\\\"" <|> escapeSequence
  where
    escapeSequence = char '\\' *> singleCharEscape
    singleCharEscape = char '\\' <|> char '\"'

numberLiteral :: CygnetParser Expression
numberLiteral = do
    str <- many1 digit <* sepAhead
    return $ ELiteral $ LInteger $ read str

endl :: CygnetParser ()
endl = void endOfLine <|> lookAhead eof

commentBegin :: CygnetParser ()
commentBegin = void $ char '#'

next :: CygnetParser ()
next = void $ many (many1 space <|> (commentBegin >> manyTill anyChar endl))

sepAhead :: CygnetParser ()
sepAhead = lookAhead (void space <|> commentBegin <|> eof)
