{-# LANGUAGE ImportQualifiedPost #-}

module Cygnet.Compiler (CompilerOptions (..), compile) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, get, modify, runStateT)

import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (intercalate, nub, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Ocelot

import Cygnet.AST

data CompilerOptions = CompilerOptions {includeDirs :: [String]}
    deriving (Show)

data CompileState = CompileState {options :: CompilerOptions, includes :: [Map String CSymbol], depth :: Int, result :: CompileResult}

type CompileResult = Text

type CompileMonad a = StateT CompileState IO a

compile :: CompilerOptions -> Module -> IO CompileResult
compile opts unit = do
    let initialState = CompileState{options = opts, includes = [], depth = 0, result = Text.empty}
    ((), state) <-
        runStateT
            ( do
                traverse_ parseCHeader (moduleIncludes unit)
                let symbols = Map.elems $ moduleSymbols unit
                let fnSymbols = filter symbolIsFunction symbols
                let fnDependencies = Set.toList $ Set.unions $ map symbolDependencies fnSymbols
                fnResolvedDeps <- traverse resolve fnDependencies
                traverse_ compileResolvedFunctionSymbol (zip fnDependencies fnResolvedDeps)
                emit "\n"
                traverse_ (emit . compileFuncDecl) fnSymbols
                emit "\n"
                traverse_ compileFuncDef fnSymbols
            )
            initialState
    return $ result state

data ResolvedSymbol
    = ResolvedCSymbol CSymbol
    | ResolvedCygnetSymbol Symbol
    | AmbiguousSymbol [ResolvedSymbol]
    | UnresolvedSymbol
    deriving (Show)

resolve :: String -> CompileMonad ResolvedSymbol
resolve name = do
    state <- get
    let cLookups = map (Map.lookup name) (includes state)
    let cResolved = map ResolvedCSymbol $ sortOn symbolName . nub $ catMaybes cLookups
    let resolved = cResolved
    if null resolved
        then return UnresolvedSymbol
        else
            if not . null $ tail resolved
                then return $ AmbiguousSymbol resolved
                else case head resolved of
                    ResolvedCSymbol csym ->
                        case symbolType csym of
                            CT_Named resolvedName -> resolve resolvedName
                            _ -> return $ head resolved
                    _ -> return $ head resolved

symbolDependencies :: Symbol -> Set String
symbolDependencies symbol =
    case symbol of
        Symbol _ _ _ (Function body _ _) -> Set.unions $ map statementDependencies body
        _ -> Set.empty

statementDependencies :: Statement -> Set String
statementDependencies st =
    case st of
        SReturn expr -> expressionDependencies expr
        SExpression expr -> expressionDependencies expr

expressionDependencies :: Expression -> Set String
expressionDependencies expr =
    case expr of
        EApply exprs -> Set.unions $ map expressionDependencies exprs
        ELiteral _ -> Set.empty
        ENamed name -> Set.singleton name

emit :: String -> CompileMonad ()
emit text = modify $ \state -> state{result = Text.append (result state) (Text.pack text)}

emitIndented :: String -> CompileMonad ()
emitIndented text = modify $ \state -> state{result = Text.append (result state) (Text.pack (concat (replicate (depth state) "    ") ++ text))}

pushIndent :: CompileMonad ()
pushIndent = modify $ \state -> state{depth = depth state + 1}

popIndent :: CompileMonad ()
popIndent = modify $ \state -> state{depth = depth state - 1}

compileResolvedFunctionSymbol :: (String, ResolvedSymbol) -> CompileMonad ()
compileResolvedFunctionSymbol (name, symbol) =
    case symbol of
        ResolvedCSymbol csym -> compileCFuncDecl csym >>= emit
        ResolvedCygnetSymbol cygsym -> emit $ compileFuncDecl cygsym
        AmbiguousSymbol _ -> error $ "Ambiguous symbol: " ++ name
        UnresolvedSymbol -> error $ "Unresolved symbol: " ++ name
  where
    compileCFuncDecl csym = do
        proto <- compileCFuncProto csym
        return $ proto ++ ";\n"
    compileCFuncProto csym =
        let cname = symbolName csym
            clinkage = symbolLinkage csym
            ctype = symbolType csym
         in case symbolClass csym of
                CS_Function ->
                    do
                        let modifier = compileCFunctionLinkage clinkage
                        retType <- compileCFuncRetType ctype
                        params <- compileCFuncParams ctype
                        return $ modifier ++ retType ++ " " ++ cname ++ "(" ++ params ++ ")"
                CS_Type -> error $ "Expected function, got type: " ++ cname
                CS_Variable -> error $ "Expected function, got variable: " ++ cname

compileCFunctionLinkage :: CSymbolLinkage -> String
compileCFunctionLinkage linkage =
    case linkage of
        CS_Private -> "static "
        CS_Public -> ""
        CS_Extern -> ""

compileCFuncRetType :: CType -> CompileMonad String
compileCFuncRetType ctype =
    case ctype of
        CT_Function ret _ _ -> compileCType ret
        _ -> return ""

compileCFuncParams :: CType -> CompileMonad String
compileCFuncParams ctype =
    case ctype of
        CT_Function _ params variadic ->
            do
                paramTypes <- traverse compileCType params
                return $ intercalate ", " (paramTypes ++ compileCVariadic variadic)
        _ -> return ""
  where
    compileCVariadic variadic
        | variadic = ["..."]
        | otherwise = []

compileCType :: CType -> CompileMonad String
compileCType ctype =
    case ctype of
        CT_Void -> return "void"
        CT_Pointer base indir ->
            do
                baseType <- compileCType base
                return $ baseType ++ concat (replicate indir "*")
        CT_Array base size -> compileCType base <&> (++ "[" ++ show size ++ "]")
        CT_Char -> return "char"
        CT_UChar -> return "unsigned char"
        CT_Short -> return "short"
        CT_UShort -> return "unsigned short"
        CT_Int -> return "int"
        CT_UInt -> return "unsigned int"
        CT_Long -> return "long"
        CT_ULong -> return "unsigned long"
        CT_LLong -> return "long long"
        CT_ULLong -> return "unsigned long long"
        CT_Float -> return "float"
        CT_Double -> return "double"
        CT_LDouble -> return "long double"
        CT_Bool -> return "_Bool"
        CT_Function{} -> return ""
        CT_Struct _ -> return ""
        CT_Union _ -> return ""
        CT_Enum _ -> return ""
        CT_Named name -> do
            resolved <- resolve name
            case resolved of
                ResolvedCSymbol csym ->
                    if symbolElaborated csym
                        then case symbolType csym of
                            CT_Struct _ -> return $ "struct " ++ symbolName csym
                            CT_Union _ -> return $ "union " ++ symbolName csym
                            CT_Enum _ -> return $ "enum " ++ symbolName csym
                            _ -> return $ symbolName csym
                        else return $ symbolName csym
                _ -> error $ "Failed to resolve C symbol: " ++ name

compileFuncProto :: Symbol -> String
compileFuncProto symbol@(Symbol saccess slinkage sname (Function fbody ftype fparams)) =
    case ftype of
        TFunction _ _ ->
            compileAccess saccess
                ++ compileTypeName (getFuncRetType ftype)
                ++ " "
                ++ compileSymbolName symbol
                ++ "("
                ++ intercalate ", " (map compileFuncParam (getFuncParams ftype fparams))
                ++ ")"
        _ -> compileFuncDecl $ Symbol saccess slinkage sname (Function fbody (TFunction TVoid ftype) ("" : fparams))

compileAccess :: Access -> String
compileAccess access = case access of
    Private -> "static "
    Public -> ""

compileSymbolName :: Symbol -> String
compileSymbolName (Symbol _ _ sname _) = sname

compileTypeName :: Type -> String
compileTypeName t =
    case t of
        TVoid -> "void"
        TString -> "char*"
        TInt -> "int"
        TFunction _ _ -> "<fn type>"

compileFuncParam :: (Type, String) -> String
compileFuncParam (ptype, pname) =
    compileTypeName ptype
        ++ (if pname /= "" then " " ++ pname else "")

compileFuncDecl :: Symbol -> String
compileFuncDecl symbol = compileFuncProto symbol ++ ";\n"

compileFuncDef :: Symbol -> CompileMonad ()
compileFuncDef symbol@(Symbol _ _ _ (Function fbody _ _)) = do
    emit $ compileFuncProto symbol ++ "\n{\n"
    pushIndent
    traverse_ compileStatement fbody
    popIndent
    emit "}\n\n"
  where
    compileStatement st =
        case st of
            SReturn expr -> compileReturn expr
            SExpression expr -> emitIndented $ compileExpression expr ++ ";\n"
    compileReturn expr =
        case expr of
            ELiteral LVoid -> emitIndented "return;\n"
            _ -> emitIndented "return " >> emit (compileExpression expr ++ ";\n")
    compileExpression expr =
        case expr of
            EApply (f : args) -> compileExpression f ++ "(" ++ intercalate ", " (map compileExpression args) ++ ")"
            EApply [] -> ""
            ELiteral literal -> compileLiteral literal
            ENamed name -> name
    compileLiteral literal =
        case literal of
            LVoid -> "void"
            LString s -> escapeString s
            LInteger i -> show i

escapeString :: String -> String
escapeString = show

parseCHeader :: String -> CompileMonad ()
parseCHeader header = do
    CompileState{options = opts} <- get
    parseResult <- liftIO $ parse (includeDirs opts) header
    case parseResult of
        Just symbols ->
            let symbolsMap = Map.fromList $ [(name, symbol) | symbol@(CSymbol{symbolName = name}) <- symbols]
             in modify $ \state -> state{includes = includes state ++ [symbolsMap]}
        _ -> error $ "Failed to parse \"" ++ header ++ "\""
