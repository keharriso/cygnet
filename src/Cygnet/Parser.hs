{-# LANGUAGE ImportQualifiedPost #-}

module Cygnet.Parser (parseCygnet) where

import Control.Monad (void, when)

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
include = topLevel >> withPos (parseKeyword "include" *> spaces *> same *> token stringLiteral <* next)

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
                same
                parseEnum access linkage
                    <|> parseFunction access linkage
            )
  where
    parseEnum access linkage = do
        next
        parseKeyword "enum"
        next
        name <- parseSymbolName
        same
        when (linkage == C && not (isValidCName name)) $
            error $
                "Invalid foreign enum name: \"" ++ name ++ "\""
        next
        sameOrIndented
        void $ token (char '=')
        next
        enumElems <- sepBy1 (parseEnumElement linkage) (try (next >> sameOrIndented >> token (char '|') >> next))
        return $ Symbol access linkage name $ TLEnum enumElems
    parseEnumElement linkage = do
        sameOrIndented
        name <- parseSymbolName
        when (linkage == C && not (isValidCName name)) $
            error $
                "Invalid foreign enum element: \"" ++ name ++ "\""
        sameOrIndented
        expr <- optionMaybe (try (next >> token (char '=') >> next >> sameOrIndented >> integerLiteral))
        return (name, expr)
    parseFunction access linkage = do
        next
        name <- parseSymbolName
        when (linkage == C && not (isValidCName name)) $
            error $
                "Invalid foreign symbol name: \"" ++ name ++ "\""
        same
        next
        sameOrIndented
        void $ char ':'
        next
        fnType <- parseType
        next
        (fnParams, fnBody) <- parseBody name
        let tl =
                if not (isConst fnBody)
                    then case fnType of
                        TFunction{} -> TLFunction fnType fnParams fnBody
                        _ -> error $ "Function body doesn't have function type: \"" ++ name ++ "\""
                    else TLConstant fnType fnBody
        return $ Symbol access linkage name tl

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
parseOperator0 = token $ (:) <$> oneOf "|" <*> many operatorChar

parseOperator1 :: CygnetParser String
parseOperator1 = try (token $ (:) <$> oneOf "&" <*> many operatorChar)

parseOperator2 :: CygnetParser String
parseOperator2 =
    try (token $ string "==")
        <|> try (token $ string "!=")

parseOperator3 :: CygnetParser String
parseOperator3 =
    try (token $ string "<")
        <|> try (token $ string ">")
        <|> try (token $ string "<=")
        <|> try (token $ string ">=")

parseOperator4 :: CygnetParser String
parseOperator4 = token $ (:) <$> oneOf "+-" <*> many operatorChar

parseOperator5 :: CygnetParser String
parseOperator5 = token $ (:) <$> oneOf "*/%" <*> many operatorChar

parseTypeName :: CygnetParser String
parseTypeName = parseSymbolName

atomicTypes :: [(String, Type)]
atomicTypes =
    [ ("void", TVoid)
    , ("byte", TByte)
    , ("ubyte", TUByte)
    , ("short", TShort)
    , ("ushort", TUShort)
    , ("int", TInt)
    , ("uint", TUInt)
    , ("long", TLong)
    , ("ulong", TULong)
    , ("llong", TLLong)
    , ("ullong", TULLong)
    , ("float", TFloat)
    , ("double", TDouble)
    , ("ldouble", TLDouble)
    , ("bool", TBool)
    ]

parseType :: CygnetParser Type
parseType = finalizeType <$> chainr1 (sameOrIndented >> parseTypeProduct <* next) (TFunction False <$ sameOrIndented <* parseKeyword "->" <* next) <?> "type"
  where
    parseTypeProduct = TProduct <$> sepBy1 (sameOrIndented >> parseTypeApply <* next) (sameOrIndented >> parseKeyword "*" >> next) <?> "product type"
    parseTypeApply = chainl1 (sameOrIndented >> parseTypeArg <* next) (return TConstructor)
    parseTypeArg = parseTypeVar <|> parseNamedType <|> parseParenType
    parseTypeVar = TVar <$> try (token (count 1 letter)) <?> "type variable"
    parseNamedType = TNamed <$> try (token parseTypeName) <?> "named type"
    parseParenType = char '(' >> next >> parseType <* next <* char ')' <* next
    finalizeType t =
        case t of
            TNamed name ->
                let atomicType = lookup name atomicTypes
                 in case atomicType of
                        Just t' -> t'
                        _ -> t
            TConstructor a b ->
                case a of
                    TNamed "ptr" -> TPtr (finalizeType b)
                    _ -> TConstructor a (finalizeType b)
            TProduct [t'] -> finalizeType t'
            TProduct ts -> TProduct $ map finalizeType ts
            TFunction v a b -> TFunction v (finalizeType a) (finalizeType b)
            _ -> t

parseBody :: String -> CygnetParser ([String], Block)
parseBody name =
    topLevel
        >> withPos
            ( do
                token (string name) >> next
                params <- parseParams
                body <- sameOrIndented >> token (char '=') >> next >> sameOrIndented >> parseDefinition
                return (params, body)
            )
  where
    parseParams = endBy (sameOrIndented >> token parseSymbolName) next
    parseDefinition = mergeLets <$> parseBlock "do" <|> ((: []) <$> parseStatement)
    mergeLets sts =
        case sts of
            (SLet as) : (SLet bs) : sts' -> mergeLets $ SLet (as ++ bs) : sts'
            st : sts' -> st : mergeLets sts'
            [] -> []
    parseStatement = (parseReturn <|> parseLet <|> parseAssign <|> parseIf "if" <|> (SExpression <$> parseExpression)) <* next
    parseBlock kw =
        let parseBlockBegin = parseKeyword kw >> next >> parseBlockBody
            parseBlockBody = sameOrIndented >> withPos (endBy1 (checkIndent >> parseStatement) next)
         in (same >> parseBlockBegin) <|> withPos parseBlockBegin
    parseReturn = withPos $ parseKeyword "return" >> next >> (SReturn <$> parseExpression)
    parseLet =
        withPos
            ( do
                parseKeyword "let" >> next >> sameOrIndented
                mutable <- (True <$ parseKeyword "mut") <|> return False
                var <- next >> sameOrIndented >> token parseSymbolName <* next
                args <- parseParams <* next
                sameOrIndented >> token (char '=') >> next
                statement <- sameOrIndented >> parseStatement
                return $ SLet [Assignment mutable var args statement]
            )
    parseAssign =
        withPos $
            try
                ( do
                    var <- token parseSymbolName <* next
                    sameOrIndented >> parseKeyword ":=" >> next
                    statement <- sameOrIndented >> parseStatement
                    return $ SAssign var statement
                )
    parseIf kw =
        withPos
            ( do
                parseKeyword kw
                next
                sameOrIndented
                condition <- parseExpression
                next
                sameOrIndented
                trueBranch <- parseBlock "then"
                next
                falseBranch <- parseElse
                next
                return $ SIf condition trueBranch falseBranch
            )
    parseElse =
        ( (sameOrIndented <|> checkIndent)
            >> (try ((: []) <$> parseIf "elseif") <|> try (parseBlock "else"))
        )
            <|> return []

    parseExpression = parseExpression0
    parseExpression0 = chainl1 parseExpression1 ((\f x y -> EApply [f, x, y]) <$> (ENamed <$> token parseOperator0 <* next))
    parseExpression1 = chainl1 parseExpression2 ((\f x y -> EApply [f, x, y]) <$> (ENamed <$> token parseOperator1 <* next))
    parseExpression2 = chainl1 parseExpression3 ((\f x y -> EApply [f, x, y]) <$> (ENamed <$> token parseOperator2 <* next))
    parseExpression3 = chainl1 parseExpression4 ((\f x y -> EApply [f, x, y]) <$> (ENamed <$> token parseOperator3 <* next))
    parseExpression4 = chainl1 parseExpression5 ((\f x y -> EApply [f, x, y]) <$> (ENamed <$> token parseOperator4 <* next))
    parseExpression5 = chainl1 parseExpression6 ((\f x y -> EApply [f, x, y]) <$> (ENamed <$> token parseOperator5 <* next))
    parseExpression6 = do
        expr <- (parseLiteral <|> parseSizeOf <|> parseApply) <* next
        asType <- parseAs
        case asType of
            Just t -> return $ ETyped expr t
            Nothing -> return expr

    parseAs = optionMaybe (parseKeyword "as" *> next *> parseType <* next)

    parseOperator = ENamed <$> token (many operatorChar)

    refChar = char '&'
    derefChar = char '@'

    parseLiteral = (voidLiteral <|> boolLiteral <|> (ELiteral . LString <$> stringLiteral) <|> numberLiteral) <* next
    parseApply = EApply <$> withPos (endBy1 parseArg next)
    parseArg = sameOrIndented >> (parseLiteral <|> parseDotted <|> parseParenExpr)
    parseSizeOf = ESizeOf <$> (parseKeyword "sizeof" *> next *> parseType)
    parseDotted = EDotted <$> (sepBy1 parseDottedPart (char '.') <* next)
    parseDottedPart = do
        ref <- optionMaybe refChar
        case ref of
            Nothing ->
                do
                    deref <- optionMaybe derefChar
                    case deref of
                        Nothing -> parseNamed <|> parseParenExpr
                        _ -> EDeref <$> parseDottedPart
            _ -> ERef <$> parseDottedPart
    parseNamed = notFollowedBy parseAnyKeyword >> (ENamed <$> token parseSymbolName)
    parseParenExpr = do
        ref <- optionMaybe refChar
        case ref of
            Nothing ->
                do
                    deref <- optionMaybe derefChar
                    case deref of
                        Nothing ->
                            do
                                expr <- char '(' >> next >> (parseOperator <|> parseExpression) <* next <* char ')' <* next
                                asType <- parseAs
                                case asType of
                                    Just t -> return $ ETyped expr t
                                    Nothing -> return expr
                        _ -> EDeref <$> parseParenExpr
            _ -> ERef <$> parseParenExpr

    parseAnyKeyword =
        let kws =
                [ "module"
                , "import"
                , "include"
                , "export"
                , "foreign"
                , "do"
                , "if"
                , "then"
                , "else"
                , "elseif"
                , "let"
                , "mut"
                , "ptr"
                , "ref"
                , "sizeof"
                , "as"
                , "enum"
                ]
                    ++ map fst atomicTypes
         in choice (map parseKeyword kws) <?> "keyword"

parseKeyword :: String -> CygnetParser ()
parseKeyword kw = void $ try (token $ string kw)

voidLiteral :: CygnetParser Expression
voidLiteral = ELiteral LVoid <$ token (try $ string "void") <?> "void"

boolLiteral :: CygnetParser Expression
boolLiteral =
    ELiteral . LBool
        <$> ((True <$ token (try $ string "true")) <|> (False <$ token (try $ string "false")))
        <?> "true or false"

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

integerLiteral :: CygnetParser Expression
integerLiteral = try hexLiteral <|> try decimalLiteral <?> "integer"
  where
    sign = (string "+" $> "") <|> string "-"
    hexLiteral =
        do
            sgn <- option "" $ try $ sign <* lookAhead digit
            digits <- token $ try (string "0x") *> many1 hexDigit
            return $ ELiteral . LHexadecimal . read $ sgn ++ "0x" ++ digits
    decimalLiteral =
        do
            sgn <- option "" $ try $ sign <* lookAhead digit
            digits <- token $ many1 digit
            return $ ELiteral . LDecimal . read $ sgn ++ digits

numberLiteral :: CygnetParser Expression
numberLiteral = integerLiteral <|> try floatLiteral <?> "number"
  where
    sign = (string "+" $> "") <|> string "-"
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
token p = p <* lookAhead (void space <|> void (oneOf "().") <|> commentBegin <|> eof)
