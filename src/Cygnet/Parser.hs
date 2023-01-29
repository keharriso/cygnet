{-# LANGUAGE ImportQualifiedPost #-}

module Cygnet.Parser (parseCygnet) where

import Control.Monad (void)

import Data.Char (chr)
import Data.Functor (($>))
import Data.Map qualified as Map

import Text.Parsec hiding (token)
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
include = topLevel >> withPos (string "include" *> spaces *> same *> token stringLiteral <* next)

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
                if linkage == C && not (isValidCName name)
                    then error $ "Invalid foreign function name: \"" ++ name ++ "\""
                    else do
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

isValidCName :: String -> Bool
isValidCName name =
    let validStartChars = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
        validCNameChars = validStartChars ++ "1234567890"
     in case name of
            [] -> False
            c : cs -> (c `elem` validStartChars) && all (`elem` validCNameChars) cs

parseAccess :: CygnetParser Access
parseAccess = (try (string "export" >> space) >> return Public) <|> return Private

parseLinkage :: CygnetParser Linkage
parseLinkage = (try (string "foreign" >> space) >> return C) <|> return Cygnet

parseSymbolName :: CygnetParser String
parseSymbolName = (:) <$> symbolStart <*> many symbolChar
  where
    symbolStart = alphaNum <|> char '_'
    symbolChar = symbolStart <|> oneOf "-'?!"

operatorChar :: CygnetParser Char
operatorChar = oneOf "+-<>=!&|:$"

parseOperator0 :: CygnetParser String
parseOperator0 = (:) <$> oneOf "+-" <*> many operatorChar

parseOperator1 :: CygnetParser String
parseOperator1 = (:) <$> oneOf "*/%" <*> many operatorChar

parseTypeName :: CygnetParser String
parseTypeName = parseSymbolName

parseType :: CygnetParser Type
parseType = do
    curriedTypes <- sepBy1 (sameOrIndented >> parseAtomicType <* next) (sameOrIndented >> string "->" >> next)
    return $ foldr1 TFunction curriedTypes
  where
    parseAtomicType =
        try (TVoid <$ string "void")
            <|> try (TBool <$ string "bool")
            <|> try (TString <$ string "string")
            <|> try (TNumber <$ string "int")
            <|> try (TNumber <$ string "double")

parseBody :: String -> CygnetParser ([String], Block)
parseBody name =
    topLevel
        >> withPos
            ( do
                token (string name) >> next
                params <- parseParams
                body <- sameOrIndented >> token (char '=') >> next >> sameOrIndented >> parseDefinition
                return (if null params then [""] else params, body)
            )
  where
    parseParams = endBy (sameOrIndented >> token parseSymbolName) next
    parseDefinition = mergeLets <$> parseBlock "do" <|> ((: []) <$> parseStatement)
    mergeLets sts =
        case sts of
            (SLet as) : (SLet bs) : sts' -> mergeLets $ SLet (as ++ bs) : sts'
            st : sts' -> st : mergeLets sts'
            [] -> []
    parseStatement = (parseReturn <|> parseLet <|> parseIf <|> (SExpression <$> parseExpression)) <* next
    parseBlock kw =
        let parseBlockBegin = parseKeyword kw >> next >> parseBlockBody
            parseBlockBody = sameOrIndented >> withPos (endBy1 (checkIndent >> parseStatement) next)
         in (same >> parseBlockBegin) <|> withPos parseBlockBegin
    parseReturn = withPos $ parseKeyword "return" >> next >> (SReturn <$> parseExpression)
    parseLet =
        withPos
            ( do
                var <- parseKeyword "let" >> next >> sameOrIndented >> token parseSymbolName <* next
                args <- parseParams <* next
                sameOrIndented >> token (char '=') >> next
                statement <- sameOrIndented >> parseStatement
                return $ SLet [Assignment var args statement]
            )
    parseIf =
        withPos
            ( do
                parseKeyword "if"
                next
                sameOrIndented
                condition <- parseExpression
                next
                sameOrIndented
                trueBranch <- parseBlock "then"
                next
                sameOrIndented
                falseBranch <- parseBlock "else" <|> return []
                next
                return $ SIf condition trueBranch falseBranch
            )

    parseExpression = parseExpression0
    parseExpression0 = chainl1 parseExpression1 ((\f x y -> EApply [f, x, y]) <$> (ENamed <$> token parseOperator0 <* next))
    parseExpression1 = chainl1 parseExpression2 ((\f x y -> EApply [f, x, y]) <$> (ENamed <$> token parseOperator1 <* next))
    parseExpression2 = (parseLiteral <|> parseApply) <* next

    parseOperator = ENamed <$> token (many operatorChar)

    parseLiteral = (voidLiteral <|> boolLiteral <|> (ELiteral . LString <$> stringLiteral) <|> numberLiteral) <* next
    parseApply = EApply <$> withPos (endBy1 parseArg next)
    parseArg = sameOrIndented >> (parseLiteral <|> parseNamed <|> parseParenExpr)
    parseNamed = notFollowedBy parseAnyKeyword >> (ENamed <$> token parseSymbolName <* next)
    parseParenExpr = char '(' >> next >> (parseOperator <|> parseExpression) <* next <* char ')' <* next
    parseAnyKeyword =
        let kws = ["module", "import", "include", "export", "foreign", "do", "if", "then", "else"]
         in choice (map parseKeyword kws) <?> "keyword"
    parseKeyword kw = void $ try (token $ string kw)

voidLiteral :: CygnetParser Expression
voidLiteral = ELiteral LVoid <$ token (try $ string "void") <?> "void"

boolLiteral :: CygnetParser Expression
boolLiteral = ELiteral . LBool <$>
    ((True <$ token (try $ string "true")) <|> (False <$ token (try $ string "false"))) <?> "true or false"

stringLiteral :: CygnetParser String
stringLiteral = token (char '"' *> many quotedChar <* char '"') <?> "string"

quotedChar :: CygnetParser Char
quotedChar = noneOf "\\\"" <|> escapeSequence
  where
    escapeSequence = char '\\' *> (singleCharEscape <|> hexChar)
    singleCharEscape =
        char 'a' $> '\a'
            <|> char 'b' $> '\b'
            <|> char 'f' $> '\f'
            <|> char 'n' $> '\n'
            <|> char 'r' $> '\r'
            <|> char 't' $> '\t'
            <|> char 'v' $> '\v'
            <|> char '\\' $> '\\'
            <|> char '\'' $> '\''
            <|> char '"' $> '"'
            <|> char '?' $> '?'
            <|> char '0' $> '\0'
    hexChar = (\hex -> chr $ read $ "0x" ++ hex) <$> (char 'x' >> count 2 hexDigit)

numberLiteral :: CygnetParser Expression
numberLiteral = hexLiteral <|> try integerLiteral <|> floatLiteral <?> "number"
  where
    sign = (string "+" $> "") <|> string "-"
    hexLiteral =
        do
            sgn <- option "" $ try $ sign <* lookAhead digit
            digits <- token $ try (string "0x") *> many1 hexDigit
            return $ ELiteral . LInteger . read $ sgn ++ "0x" ++ digits
    integerLiteral =
        do
            sgn <- option "" $ try $ sign <* lookAhead digit
            digits <- token $ many1 digit
            return $ ELiteral . LInteger . read $ sgn ++ digits
    floatLiteral =
        do
            sgn <- option "" $ try $ sign <* lookAhead digit
            left <- many1 digit
            point <- option "" $ string "." <* lookAhead digit
            right <- many digit
            e <- option "" $ string "e" <* lookAhead (void digit <|> void sign)
            esgn <- option "" $ sign <* lookAhead digit
            eexp <- token $ many digit
            return $ ELiteral . LFloat . read $ sgn ++ left ++ point ++ right ++ e ++ esgn ++ eexp

endl :: CygnetParser ()
endl = void endOfLine <|> lookAhead eof

commentBegin :: CygnetParser ()
commentBegin = void $ char '#'

next :: CygnetParser ()
next = void $ many $ void space <|> void (commentBegin >> manyTill anyChar endl)

token :: CygnetParser a -> CygnetParser a
token p = p <* lookAhead (void space <|> void (oneOf "()") <|> commentBegin <|> eof)
