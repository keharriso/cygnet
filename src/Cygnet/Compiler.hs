{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Cygnet.Compiler (CompilerOptions (..), NumericLimits (..), compile) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, get, gets, modify, runStateT)

import Data.Char (ord)
import Data.Foldable (foldl', traverse_)
import Data.Functor ((<&>))
import Data.List (intercalate, nub, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Numeric (showHex)

import Ocelot

import Cygnet.AST

data NumericLimits = NumericLimits
    { charBit :: Integer
    , scharMin :: Integer
    , scharMax :: Integer
    , ucharMax :: Integer
    , charMin :: Integer
    , charMax :: Integer
    , mbLenMax :: Integer
    , shrtMin :: Integer
    , shrtMax :: Integer
    , ushrtMax :: Integer
    , intMin :: Integer
    , intMax :: Integer
    , uintMax :: Integer
    , longMin :: Integer
    , longMax :: Integer
    , ulongMax :: Integer
    , llongMin :: Integer
    , llongMax :: Integer
    , ullongMax :: Integer
    }
    deriving (Show)

data CompilerOptions = CompilerOptions
    { includeDirs :: [FilePath]
    , verbose :: Bool
    , numericLimits :: NumericLimits
    }
    deriving (Show)

data CompileState = CompileState
    { compilerOptions :: CompilerOptions
    , compilerImports :: [Import]
    , compilerIncludes :: [Map String CSymbol]
    , compilerLocals :: [Map String LocalVariable]
    , compilerAnonymous :: [(String, Type)]
    , compilerDepth :: Int
    , compilerNextVar :: Int
    , compilerNextTypeVar :: Int
    , compilerResult :: CompileResult
    }

type Import = Module

data LocalVariable = LocalVariable Type Mutable
    deriving (Eq, Show)

type CompileResult = Text

type CompileMonad a = StateT CompileState IO a

compile :: CompilerOptions -> Module -> IO CompileResult
compile opts unit = do
    let initialState =
            CompileState
                { compilerOptions = opts
                , compilerImports = [unit]
                , compilerIncludes = []
                , compilerLocals = []
                , compilerAnonymous = []
                , compilerDepth = 0
                , compilerNextVar = 0
                , compilerNextTypeVar = 0
                , compilerResult = Text.empty
                }
    ((), st) <-
        runStateT
            ( do
                parseCHeader "stddef.h"
                traverse_ parseCHeader (moduleIncludes unit)
                let symbols = Map.elems $ moduleSymbols unit
                let constSymbols = filter symbolIsConstant symbols
                unless
                    (null constSymbols)
                    (traverse_ compileSymbolDef constSymbols >> emit "\n")
                let fnSymbols = filter symbolIsFunction symbols
                fnDependencies <- Set.toList . Set.unions <$> traverse symbolDependencies fnSymbols
                fnResolvedDeps <- traverse resolve fnDependencies
                let isExternal dep =
                        case dep of
                            BuiltInValue _ -> False
                            CygnetSymbolValue sym -> sym `notElem` moduleSymbols unit
                            _ -> True
                let externalDeps = filter isExternal fnResolvedDeps
                unless
                    (null externalDeps)
                    (traverse_ compileResolvedFunctionSymbol externalDeps >> emit "\n")
                fnDecls <- traverse compileSymbolDecl fnSymbols
                unless
                    (null fnDecls)
                    (traverse_ emit fnDecls >> emit "\n")
                traverse_ compileSymbolDef fnSymbols
            )
            initialState
    return $ compilerResult st

data Value
    = TemporaryValue Int Type
    | LocalValue String LocalVariable
    | AnonymousValue String Type
    | PartialValue Value [Value]
    | BuiltInValue BuiltIn
    | CSymbolValue CSymbol
    | CygnetSymbolValue Symbol
    | AmbiguousValue String [Value]
    | UnresolvedValue String
    | NoValue
    deriving (Eq, Show)

mangleName :: String -> String
mangleName = concatMap mangleChar
  where
    isValidCSymbolChar c =
        c `elem` "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
    mangleChar c
        | c == '_' = "__"
        | isValidCSymbolChar c = [c]
        | otherwise = "_" ++ padL '0' 2 (showHex (ord c) "")
    padL p s l
        | length l >= s = l
        | otherwise = replicate (s - length l) p ++ l

getCygnetTopLevelName :: String -> String
getCygnetTopLevelName name = "cyg_tl_" ++ mangleName name

getAnonymousParameterNames :: [String]
getAnonymousParameterNames = map (\n -> "_cyg_anon_" ++ show (n :: Int)) [0 ..]

compileValue :: Value -> String
compileValue value =
    case value of
        TemporaryValue varId _ -> "_cyg_temp_" ++ show varId
        LocalValue var _ -> "_cyg_local_" ++ mangleName var
        AnonymousValue name _ -> name
        PartialValue _ _ -> undefined
        BuiltInValue _ -> undefined
        CSymbolValue csym -> symbolName csym
        CygnetSymbolValue (Symbol _ linkage name _) ->
            case linkage of
                Cygnet -> getCygnetTopLevelName name
                C -> name
        AmbiguousValue name _ -> error $ "Ambiguous symbol: \"" ++ name ++ "\""
        UnresolvedValue name -> error $ "Unresolved symbol: \"" ++ name ++ "\""
        NoValue -> error "No value"

compileApplyValue :: Value -> [Expression] -> CompileMonad String
compileApplyValue value args =
    getArity value
        >>= \arity ->
            if length args < arity
                then compilePartialApply value args
                else
                    if length args == arity || isVariadic value
                        then compileCompleteApply value args
                        else error "Function applied to too many arguments"
  where
    compileCompleteApply f exprs =
        case f of
            BuiltInValue builtIn -> builtInCompile builtIn exprs
            _ ->
                if null args
                    then return $ compileValue f
                    else do
                        compiledExprs <- traverse compileExpression (filter (/= ELiteral LVoid) args)
                        let compiledValues = map compileValue compiledExprs
                        return $ compileValue f ++ "(" ++ intercalate ", " compiledValues ++ ")"
    compilePartialApply _ _ = undefined

getType :: Value -> Type
getType v =
    case v of
        TemporaryValue _ t -> t
        LocalValue _ (LocalVariable t _) -> t
        AnonymousValue _ t -> t
        PartialValue f args ->
            let types = map getType (f : args)
             in fromJust $ getApplyType types $ isVariadic f
        BuiltInValue (BuiltIn{builtInType = t}) -> t
        CSymbolValue csym -> convertCToCygnet $ symbolType csym
        CygnetSymbolValue sym -> symbolGetType sym
        AmbiguousValue name _ -> error $ "Ambiguous symbol: \"" ++ name ++ "\""
        UnresolvedValue name -> error $ "Unresolved symbol: \"" ++ name ++ "\""
        NoValue -> error "No value"

getArity :: Value -> CompileMonad Int
getArity v =
    case v of
        TemporaryValue _ t -> typeArity t
        LocalValue _ (LocalVariable t _) -> typeArity t
        AnonymousValue _ t -> typeArity t
        PartialValue f args -> (-) <$> getArity f <*> return (length args)
        BuiltInValue builtIn -> return $ builtInArity builtIn
        CSymbolValue (CSymbol{symbolType = CT_Function _ params _}) -> return $ length params
        CygnetSymbolValue (Symbol _ _ _ (TLFunction t _ _)) -> typeArity t
        CygnetSymbolValue (Symbol _ _ _ (TLConstant _ _)) -> return 0
        _ -> return 0
  where
    typeArity t =
        case t of
            TFunction _ a _ -> productSize a
            TNamed name -> resolve name >>= getArity
            _ -> return 0
    productSize t =
        case t of
            TProduct ts -> return $ length ts
            _ -> return 1

isVariadic :: Value -> Bool
isVariadic v =
    case v of
        PartialValue v' _ -> isVariadic v'
        CSymbolValue (CSymbol{symbolType = CT_Function _ _ variadic}) -> variadic
        _ -> False

data BuiltIn = BuiltIn
    { builtInName :: String
    , builtInType :: Type
    , builtInArity :: Int
    , builtInCompile :: [Expression] -> CompileMonad String
    }

instance Eq BuiltIn where
    (==) :: BuiltIn -> BuiltIn -> Bool
    (==) a b = builtInName a == builtInName b && builtInType a == builtInType b

instance Show BuiltIn where
    show :: BuiltIn -> String
    show (BuiltIn{builtInName = name, builtInType = t}) = "BuiltIn(" ++ name ++ " : " ++ show t ++ ")"

builtIns :: Map String BuiltIn
builtIns =
    Map.fromList
        [ binaryBuiltIn "*" TInt TInt TInt $ \a b -> "(" ++ a ++ " * " ++ b ++ ")"
        , binaryBuiltIn "/" TInt TInt TInt $ \a b -> "(" ++ a ++ " / " ++ b ++ ")"
        , binaryBuiltIn "%" TInt TInt TInt $ \a b -> "(" ++ a ++ " % " ++ b ++ ")"
        , binaryBuiltIn "+" TInt TInt TInt $ \a b -> "(" ++ a ++ " + " ++ b ++ ")"
        , binaryBuiltIn "-" TInt TInt TInt $ \a b -> "(" ++ a ++ " - " ++ b ++ ")"
        , binaryBuiltIn "<" TInt TInt TBool $ \a b -> "(" ++ a ++ " < " ++ b ++ ")"
        , binaryBuiltIn "<=" TInt TInt TBool $ \a b -> "(" ++ a ++ " <= " ++ b ++ ")"
        , binaryBuiltIn ">" TInt TInt TBool $ \a b -> "(" ++ a ++ " > " ++ b ++ ")"
        , binaryBuiltIn ">=" TInt TInt TBool $ \a b -> "(" ++ a ++ " >= " ++ b ++ ")"
        , binaryBuiltIn "==" TInt TInt TBool $ \a b -> "(" ++ a ++ " == " ++ b ++ ")"
        , binaryBuiltIn "!=" TInt TInt TBool $ \a b -> "(" ++ a ++ " != " ++ b ++ ")"
        , lazyBinaryAnd "&&"
        , lazyBinaryOr "||"
        ]
  where
    binaryBuiltIn name a b c def =
        let compileBinary exprs =
                do
                    args <- traverse compileExpression exprs
                    let values = map compileValue args
                    case values of
                        [x, y] -> return $ def x y
                        _ -> error $ "Binary operator applied to " ++ show (length exprs) ++ " arguments"
         in ( name
            , BuiltIn
                { builtInName = name
                , builtInType = TFunction False a (TFunction False b c)
                , builtInArity = 2
                , builtInCompile = compileBinary
                }
            )
    lazyBinaryAnd name =
        let compileAnd exprs =
                case exprs of
                    [a, b] -> do
                        a' <- compileExpression a
                        result <- genVar TBool
                        boolName <- compileTypeName TBool
                        emitIndented $ boolName ++ " " ++ compileValue result ++ " = 0;\n"
                        emitIndented $ "if (" ++ compileValue a' ++ ")\n"
                        emitIndented "{\n"
                        pushIndent
                        b' <- compileExpression b
                        emitIndented $ compileValue result ++ " = " ++ compileValue b' ++ ";\n"
                        popIndent
                        emitIndented "}\n"
                        return $ compileValue result
                    _ -> error $ "Binary operator applied to " ++ show (length exprs) ++ " arguments"
         in ( name
            , BuiltIn
                { builtInName = name
                , builtInType = TFunction False TBool (TFunction False TBool TBool)
                , builtInArity = 2
                , builtInCompile = compileAnd
                }
            )
    lazyBinaryOr name =
        let compileOr exprs =
                case exprs of
                    [a, b] -> do
                        a' <- compileExpression a
                        result <- genVar TBool
                        boolName <- compileTypeName TBool
                        emitIndented $ boolName ++ " " ++ compileValue result ++ " = 1;\n"
                        emitIndented $ "if (!" ++ compileValue a' ++ ")\n"
                        emitIndented "{\n"
                        pushIndent
                        b' <- compileExpression b
                        emitIndented $ compileValue result ++ " = " ++ compileValue b' ++ ";\n"
                        popIndent
                        emitIndented "}\n"
                        return $ compileValue result
                    _ -> error $ "Binary operator applied to " ++ show (length exprs) ++ " arguments"
         in ( name
            , BuiltIn
                { builtInName = name
                , builtInType = TFunction False TBool (TFunction False TBool TBool)
                , builtInArity = 2
                , builtInCompile = compileOr
                }
            )

resolve :: String -> CompileMonad Value
resolve name = do
    anon <- getAnonymous name
    case anon of
        AnonymousValue _ _ -> return anon
        _ -> do
            local <- getLocal name
            case local of
                LocalValue _ _ -> return local
                AmbiguousValue _ _ -> return local
                _ -> do
                    let maybeBuiltIn = Map.lookup name builtIns
                    case maybeBuiltIn of
                        Just builtIn -> return $ BuiltInValue builtIn
                        _ -> do
                            st <- get
                            let cygLookups = map (Map.lookup name . moduleSymbols) (compilerImports st)
                            let cygResolved =
                                    map CygnetSymbolValue $
                                        sortOn (\(Symbol _ _ name' _) -> name') . nub $
                                            catMaybes cygLookups
                            let cLookups = map (Map.lookup name) (compilerIncludes st)
                            let cResolved =
                                    map CSymbolValue $
                                        sortOn symbolName . nub $
                                            catMaybes cLookups
                            let resolved = cygResolved ++ cResolved
                            if null resolved
                                then return $ UnresolvedValue name
                                else
                                    if not . null $ tail resolved
                                        then return $ AmbiguousValue name resolved
                                        else case head resolved of
                                            CSymbolValue csym ->
                                                case symbolType csym of
                                                    CT_Named resolvedName -> resolve resolvedName
                                                    _ -> return $ head resolved
                                            _ -> return $ head resolved

symbolDependencies :: Symbol -> CompileMonad (Set String)
symbolDependencies symbol =
    case symbol of
        Symbol _ _ _ (TLFunction ftype params fbody) -> functionDependencies ftype params fbody
        _ -> return Set.empty

functionDependencies :: Type -> [String] -> Block -> CompileMonad (Set String)
functionDependencies ftype params fbody = do
    resetAnonymous
    pushLocalBlock
    let fparams = getFuncParams ftype params
    traverse_ assignVar fparams
    deps <- blockDependencies fbody
    popLocalBlock
    return deps

blockDependencies :: Block -> CompileMonad (Set String)
blockDependencies block = do
    pushLocalBlock
    dependencySets <- traverse statementDependencies block
    popLocalBlock
    return $ Set.unions dependencySets

statementDependencies :: Statement -> CompileMonad (Set String)
statementDependencies st =
    case st of
        SReturn expr -> expressionDependencies expr
        SExpression expr -> expressionDependencies expr
        SLet assignments -> do
            traverse_ (\(Assignment mut f _ _) -> putLocal f $ LocalVariable TVoid mut) assignments
            Set.unions <$> traverse assignmentDependencies assignments
        SAssign _ st' -> statementDependencies st'
        SIf cond tBranch fBranch -> do
            condDeps <- expressionDependencies cond
            tBranchDeps <- blockDependencies tBranch
            fBranchDeps <- blockDependencies fBranch
            return $ Set.unions [condDeps, tBranchDeps, fBranchDeps]
  where
    assignmentDependencies (Assignment mut _ args st') =
        do
            pushLocalBlock
            traverse_ (\arg -> putLocal arg $ LocalVariable TVoid mut) args
            deps <- statementDependencies st'
            popLocalBlock
            return deps

expressionDependencies :: Expression -> CompileMonad (Set String)
expressionDependencies expr =
    case expr of
        EApply exprs -> Set.unions <$> traverse expressionDependencies exprs
        ELiteral _ -> return Set.empty
        ENamed name -> do
            resolved <- getLocal name
            case resolved of
                LocalValue _ _ -> return Set.empty
                _ -> return $ Set.singleton name
        EDeref expr' -> expressionDependencies expr'
        EDotted exprs ->
            case exprs of
                [] -> return Set.empty
                e : _ -> expressionDependencies e
        ETyped expr' _ -> expressionDependencies expr'
        ESizeOf _ -> return Set.empty

emit :: String -> CompileMonad ()
emit text = modify $ \st -> st{compilerResult = Text.append (compilerResult st) (Text.pack text)}

emitIndented :: String -> CompileMonad ()
emitIndented text = modify $ \st ->
    st
        { compilerResult =
            Text.append (compilerResult st) (Text.pack (concat (replicate (compilerDepth st) "    ") ++ text))
        }

pushIndent :: CompileMonad ()
pushIndent = modify $ \st -> st{compilerDepth = compilerDepth st + 1}

popIndent :: CompileMonad ()
popIndent = modify $ \st -> st{compilerDepth = compilerDepth st - 1}

pushLocalBlock :: CompileMonad ()
pushLocalBlock = modify $ \st -> st{compilerLocals = Map.empty : compilerLocals st}

popLocalBlock :: CompileMonad ()
popLocalBlock = modify $ \st -> st{compilerLocals = tail (compilerLocals st)}

putLocal :: String -> LocalVariable -> CompileMonad ()
putLocal var varType = do
    (localBlock : parentBlocks) <- gets compilerLocals
    let local = Map.lookup var localBlock
    case local of
        Nothing -> modify $ \st -> st{compilerLocals = Map.insert var varType localBlock : parentBlocks}
        _ -> error $ "Duplicate variable name: " ++ var

setLocal :: String -> LocalVariable -> CompileMonad ()
setLocal var varType = do
    (localBlock : parentBlocks) <- gets compilerLocals
    modify $ \st -> st{compilerLocals = Map.insert var varType localBlock : parentBlocks}

getLocal :: String -> CompileMonad Value
getLocal var = do
    st <- get
    let localLookups = mapMaybe (Map.lookup var) (compilerLocals st)
    let localResolved = map (LocalValue var) (nub localLookups)
    case localResolved of
        local : _ -> return local
        [] -> return $ UnresolvedValue var

genVar :: Type -> CompileMonad Value
genVar t = do
    varId <- gets compilerNextVar
    modify $ \st -> st{compilerNextVar = varId + 1}
    return $ TemporaryValue varId t

genTypeVar :: CompileMonad String
genTypeVar = do
    typeVarId <- gets compilerNextTypeVar
    modify $ \st -> st{compilerNextTypeVar = typeVarId + 1}
    return $ "a" ++ show typeVarId

resetVars :: CompileMonad ()
resetVars = modify $ \st -> st{compilerNextVar = 0, compilerNextTypeVar = 0}

resetAnonymous :: CompileMonad ()
resetAnonymous = modify $ \st -> st{compilerAnonymous = []}

setAnonymous :: String -> Type -> CompileMonad ()
setAnonymous name t = modify $ \st -> st{compilerAnonymous = (name, t) : compilerAnonymous st}

getAnonymous :: String -> CompileMonad Value
getAnonymous name = do
    anons <- gets compilerAnonymous
    case lookup name anons of
        Just t -> return $ AnonymousValue name t
        Nothing -> return NoValue

compileResolvedFunctionSymbol :: Value -> CompileMonad ()
compileResolvedFunctionSymbol value =
    case value of
        TemporaryValue _ _ -> error "Expected top-level function, got temporary variable"
        LocalValue var _ -> error $ "Expected top-level function, got local symbol: \"" ++ var ++ "\""
        AnonymousValue _ _ -> error "Expected top-level function, got anonymous parameter"
        PartialValue _ _ -> error "Expected top-level function, got partial application"
        BuiltInValue _ -> return ()
        CSymbolValue csym -> compileCFuncDecl csym >>= emit
        CygnetSymbolValue cygsym -> compileSymbolDecl cygsym >>= emit
        AmbiguousValue name _ -> error $ "Ambiguous symbol: \"" ++ name ++ "\""
        UnresolvedValue name -> error $ "Unresolved symbol: \"" ++ name ++ "\""
        NoValue -> error "No value"
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
        CT_SChar -> return "signed char"
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
                CSymbolValue csym ->
                    if symbolElaborated csym
                        then case symbolType csym of
                            CT_Struct _ -> return $ "struct " ++ symbolName csym
                            CT_Union _ -> return $ "union " ++ symbolName csym
                            CT_Enum _ -> return $ "enum " ++ symbolName csym
                            _ -> return $ symbolName csym
                        else return $ symbolName csym
                _ -> error $ "Failed to resolve C symbol: " ++ name

convertCToCygnet :: CType -> Type
convertCToCygnet ctype =
    case ctype of
        CT_Void -> TVoid
        CT_Pointer t n ->
            let buildPtrType n' =
                    if n' == 0
                        then convertCToCygnet t
                        else TPtr (buildPtrType (n' - 1))
             in buildPtrType n
        CT_Array t _ -> convertCToCygnet (CT_Pointer t 1)
        CT_SChar -> TByte
        CT_UChar -> TUByte
        CT_Short -> TShort
        CT_UShort -> TUShort
        CT_Int -> TInt
        CT_UInt -> TUInt
        CT_Long -> TLong
        CT_ULong -> TULong
        CT_LLong -> TLLong
        CT_ULLong -> TULLong
        CT_Float -> TFloat
        CT_Double -> TDouble
        CT_LDouble -> TLDouble
        CT_Bool -> TBool
        CT_Function r ps v ->
            let funcType =
                    buildFuncType $
                        map convertCToCygnet $
                            if null ps then [CT_Void, r] else ps ++ [r]
             in case funcType of
                    TFunction _ a b -> TFunction v a b
                    _ -> funcType
        CT_Struct _ -> undefined
        CT_Union _ -> undefined
        CT_Enum _ -> undefined
        CT_Named n -> TNamed n
  where
    buildFuncType curriedTypes =
        case curriedTypes of
            [] -> TVoid
            [t] -> t
            (t : ts) -> TFunction False t (buildFuncType ts)

compileFuncProto :: Access -> Linkage -> String -> Type -> [Value] -> CompileMonad String
compileFuncProto access linkage name ftype params =
    case ftype of
        TFunction{} ->
            do
                typeName <- compileTypeName (fromJust $ getPartialType (length params) ftype)
                funcParams <- compileFuncParams params
                return
                    ( compileAccess access
                        ++ typeName
                        ++ " "
                        ++ name
                        ++ "("
                        ++ intercalate ", " funcParams
                        ++ ")"
                    )
        _ -> compileFuncProto access linkage name (TFunction False TVoid ftype) params

compileAccess :: Access -> String
compileAccess access = case access of
    Private -> "static "
    Public -> ""

compileTypeName :: Type -> CompileMonad String
compileTypeName t =
    case t of
        TVoid -> return "void"
        TPtr b -> (++ "*") <$> compileTypeName b
        TByte -> return "signed char"
        TUByte -> return "unsigned char"
        TShort -> return "short"
        TUShort -> return "unsigned short"
        TInt -> return "int"
        TUInt -> return "unsigned int"
        TLong -> return "long"
        TULong -> return "unsigned long"
        TLLong -> return "long long"
        TULLong -> return "unsigned long long"
        TFloat -> return "float"
        TDouble -> return "double"
        TLDouble -> return "long double"
        TBool -> return "int"
        TProduct{} -> return $ "<prod type> " ++ show t
        TFunction{} -> return $ "<fn type> " ++ show t
        TNamed name -> resolve name >>= compileTypeName . getType
        TConstructor _ _ -> return $ "<type constructor> " ++ show t
        TVar a -> return $ "<type var " ++ a ++ ">"

compileFuncParam :: Value -> CompileMonad String
compileFuncParam pval = do
    let ptype = getType pval
    typeName <- compileTypeName ptype
    if ptype == TVoid
        then return typeName
        else return $ typeName ++ " " ++ compileValue pval

compileFuncParams :: [Value] -> CompileMonad [String]
compileFuncParams params =
    case params of
        [] -> return ["void"]
        _ -> traverse compileFuncParam params

compileSymbolDecl :: Symbol -> CompileMonad String
compileSymbolDecl symbol = case symbol of
    Symbol access linkage _ (TLFunction ftype params _) ->
        compileFuncDecl access linkage (compileValue $ CygnetSymbolValue symbol) ftype params
    Symbol _ _ name _ -> error $ "Attempting to compile declaration of non-function: " ++ name

compileSymbolDef :: Symbol -> CompileMonad ()
compileSymbolDef symbol = case symbol of
    Symbol access linkage _ (TLFunction ftype params fbody) ->
        compileFuncDef access linkage (compileValue $ CygnetSymbolValue symbol) fbody ftype params
    Symbol access linkage _ (TLConstant ctype cbody) ->
        compileConstDef access linkage (compileValue $ CygnetSymbolValue symbol) cbody ctype
    Symbol _ _ _ (TLEnum _) ->
        compileEnum symbol

compileFuncDecl :: Access -> Linkage -> String -> Type -> [String] -> CompileMonad String
compileFuncDecl access linkage name ftype params =
    (++ ";\n") <$> compileFuncProto access linkage name ftype (getFuncParams ftype params)

compileFuncDef :: Access -> Linkage -> String -> Block -> Type -> [String] -> CompileMonad ()
compileFuncDef access linkage name fbody ftype params = do
    let paramCount = length params
    uncurriedParams <- filter (\v -> getType v /= TVoid) <$> beginFuncDef
    let anonCount = length uncurriedParams - paramCount
    let fbody' = decurryFunction anonCount fbody
    let retType = fromJust $ getPartialType (length params) ftype
    value <- compileBlock fbody'
    if retType /= TVoid && value /= NoValue
        then emitIndented ("return " ++ compileValue value ++ ";\n") >> endFuncDef
        else endFuncDef
  where
    beginFuncDef = do
        resetVars
        resetAnonymous
        pushLocalBlock
        let fparams = getFuncParams ftype params
        fproto <- compileFuncProto access linkage name ftype fparams
        emit $ fproto ++ "\n{\n"
        traverse_ assignVar fparams
        pushIndent
        return fparams
    endFuncDef = do
        popIndent
        emit "}\n\n"
        popLocalBlock

compileConstDef :: Access -> Linkage -> String -> Block -> Type -> CompileMonad ()
compileConstDef _ _ name cbody ctype =
    do
        typeName <- compileTypeName ctype
        constVal <-
            case cbody of
                [SExpression (ELiteral (LBool x))] -> return (if x then "1" else "0")
                [SExpression (ELiteral (LDecimal x))] -> return (showDecimalLiteral ctype x)
                [SExpression (ELiteral (LHexadecimal x))] -> return (showHexLiteral ctype x)
                [SExpression (ELiteral (LFloat x))] -> return (show x)
                [SExpression (ESizeOf t)] -> do
                    argTypeName <- compileTypeName t
                    return ("sizeof(" ++ argTypeName ++ ")")
                _ -> error "Compiling non-constant as a constant"
        emitIndented $ "const " ++ typeName ++ " " ++ name ++ " = " ++ constVal ++ ";\n"

compileEnum :: Symbol -> CompileMonad ()
compileEnum symbol@(Symbol _ linkage name (TLEnum values)) =
    do
        emit $ "enum " ++ compileValue (CygnetSymbolValue symbol) ++ "\n"
        emit "{\n"
        pushIndent
        traverse_ compileEnumValue values
        popIndent
        emit "};\n"
  where
    compileEnumValue (element, expr) =
        do
            let elemName =
                    if linkage == C
                        then element
                        else getCygnetTopLevelName $ name ++ "." ++ element
            emitIndented elemName
            case expr of
                Just (ELiteral (LDecimal i)) -> emit $ " = " ++ showDecimalLiteral TInt i ++ ",\n"
                Just (ELiteral (LHexadecimal i)) -> emit $ " = " ++ showHexLiteral TInt i ++ ",\n"
                _ -> emit ",\n"
compileEnum _ = error "Compiling non-enum as an enum"

assignVar :: Value -> CompileMonad ()
assignVar value =
    case value of
        LocalValue p t -> putLocal p t
        AnonymousValue i t -> setAnonymous i t
        _ -> error "Unsupported parameter value"

decurryFunction :: Int -> Block -> Block
decurryFunction anonArgs block =
    if anonArgs == 0
        then block
        else case block of
            [] -> []
            [st] -> [decurryStatement anonArgs st]
            (st : sts) ->
                case st of
                    SReturn expr -> SReturn (decurryExpr anonArgs expr) : decurryFunction anonArgs sts
                    _ -> st : decurryFunction anonArgs sts
  where
    decurryStatement aargs st =
        case st of
            SReturn expr -> SReturn (decurryExpr aargs expr)
            SIf expr t e -> SIf expr (decurryFunction aargs t) (decurryFunction aargs e)
            SExpression expr -> SExpression (decurryExpr aargs expr)
            _ -> st
    decurryExpr aargs expr =
        case expr of
            EApply exprs -> EApply $ exprs ++ map ENamed (take aargs getAnonymousParameterNames)
            ELiteral _ -> error $ "Decurrying literal expression: " ++ show expr
            ENamed _ -> error $ "Decurrying named expression: " ++ show expr
            EDeref _ -> error $ "Decurrying deref expression: " ++ show expr
            EDotted _ -> error $ "Decurrying dotted expression: " ++ show expr
            ETyped expr' _ -> decurryExpr aargs expr'
            ESizeOf _ -> error "Decurrying sizeof expression"

compileBlock :: Block -> CompileMonad Value
compileBlock blk = getLastValue <$> traverse compileStatement blk
  where
    getLastValue values =
        case values of
            [] -> NoValue
            _ -> last values

compileStatement :: Statement -> CompileMonad Value
compileStatement st =
    case st of
        SReturn expr -> compileReturn expr
        SLet assignments -> compileLet assignments
        SAssign var st' -> compileAssign var st'
        SIf cond t e -> compileIf cond t e
        SExpression expr -> compileExpression expr

compileReturn :: Expression -> CompileMonad Value
compileReturn expr =
    case expr of
        ELiteral LVoid -> emitIndented "return;\n" >> return NoValue
        _ ->
            compileExpression expr >>= \var ->
                emitIndented ("return " ++ compileValue var ++ ";\n") >> return NoValue

compileLet :: [Assignment] -> CompileMonad Value
compileLet assignments =
    do
        let quantifyTypeVars vars =
                do
                    tvars <- traverse (const genTypeVar) vars
                    traverse_ (\((mut, var), tvar) -> putLocal var (LocalVariable (TVar tvar) mut)) (zip vars tvars)
        let getAssignmentType (Assignment mut _ args st) =
                do
                    pushLocalBlock
                    quantifyTypeVars $ map (False,) args
                    t <- getStatementType st
                    popLocalBlock
                    return $ LocalVariable t mut
        let getAssignmentTypes as =
                do
                    let vars = map (\(Assignment mut var _ _) -> (mut, var)) as
                    quantifyTypeVars vars
                    traverse getAssignmentType as
        let compileAssignment (Assignment _ var args st, assignmentType@(LocalVariable at _)) =
                do
                    pushLocalBlock
                    quantifyTypeVars $ map (False,) args
                    valVar <- compileStatement st
                    popLocalBlock
                    case valVar of
                        NoValue -> error $ "Assigning void to variable \"" ++ var ++ "\""
                        _ ->
                            do
                                let assignVal = LocalValue var assignmentType
                                setLocal var assignmentType
                                let varName = compileValue assignVal
                                typeName <- compileTypeName at
                                let varDecl = typeName ++ " " ++ varName
                                emitIndented $ varDecl ++ " = " ++ compileValue valVar ++ ";\n"
        assignmentTypes <- getAssignmentTypes assignments
        traverse_ compileAssignment (zip assignments assignmentTypes)
        return NoValue

compileAssign :: String -> Statement -> CompileMonad Value
compileAssign var st = do
    assignVal <- resolve var
    case assignVal of
        LocalValue _ (LocalVariable _ mut) ->
            if mut
                then do
                    stVar <- compileStatement st
                    emitIndented $ compileValue assignVal ++ " = " ++ compileValue stVar ++ ";\n"
                    return NoValue
                else error $ "Assignment to non-mutable variable \"" ++ var ++ "\""
        _ -> error $ "Assignment to non-variable \"" ++ var ++ "\""

compileIf :: Expression -> Block -> Block -> CompileMonad Value
compileIf cond t e = do
    resultType <- getStatementType $ SIf cond t e
    resultVar <-
        case resultType of
            TVoid -> return NoValue
            _ ->
                do
                    var <- genVar resultType
                    typeName <- compileTypeName resultType
                    emitIndented $ typeName ++ " " ++ compileValue var ++ ";\n"
                    return var
    condVal <- compileExpression cond
    emitIndented $ "if (" ++ compileValue condVal ++ ")\n"
    emitIndented "{\n"
    pushIndent
    thenResult <- compileBlock t
    when
        (resultType /= TVoid && resultVar /= NoValue)
        (emitIndented $ compileValue resultVar ++ " = " ++ compileValue thenResult ++ ";\n")
    unless (null e) $ do
        popIndent
        emitIndented "}\n"
        emitIndented "else\n"
        emitIndented "{\n"
        pushIndent
        elseResult <- compileBlock e
        when
            (resultType /= TVoid && resultVar /= NoValue)
            (emitIndented $ compileValue resultVar ++ " = " ++ compileValue elseResult ++ ";\n")
    popIndent
    emitIndented "}\n"
    return resultVar

compileExpression :: Expression -> CompileMonad Value
compileExpression expr =
    case expr of
        EApply exprs ->
            case exprs of
                [] -> return NoValue
                _ -> compileApply exprs
        ELiteral literal -> compileLiteral literal
        ENamed name ->
            do
                resolved <- resolve name
                case resolved of
                    AmbiguousValue name' _ -> error $ "Ambiguous symbol: \"" ++ name' ++ "\""
                    UnresolvedValue name' -> error $ "Unresolved symbol: \"" ++ name' ++ "\""
                    _ -> return resolved
        ETyped (ELiteral (LDecimal n)) t ->
            do
                typeName <- compileTypeName t
                compileAtomicLiteral typeName t (showDecimalLiteral t n)
        ETyped (ELiteral (LHexadecimal n)) t ->
            do
                typeName <- compileTypeName t
                compileAtomicLiteral typeName t (showHexLiteral t n)
        ETyped x t ->
            do
                val <- compileExpression x
                var <- genVar t
                typeName <- compileTypeName t
                emitIndented $ typeName ++ " " ++ compileValue var ++ " = (" ++ typeName ++ ")" ++ compileValue val ++ ";\n"
                return var
        ESizeOf t ->
            do
                typeName <- compileTypeName t
                compileAtomicLiteral "size_t" (TNamed "size_t") ("sizeof(" ++ typeName ++ ")")

compileApply :: [Expression] -> CompileMonad Value
compileApply exprs = do
    resultType <- getExpressionType $ EApply exprs
    f <- compileExpression $ head exprs
    (left, result) <- case resultType of
        TVoid -> return ("", NoValue)
        _ ->
            genVar resultType >>= \var ->
                compileTypeName resultType >>= \typeName ->
                    return (typeName ++ " " ++ compileValue var ++ " = ", var)
    right <- compileApplyValue f $ tail exprs
    emitIndented $ left ++ right ++ ";\n"
    return result

compileLiteral :: Literal -> CompileMonad Value
compileLiteral literal =
    case literal of
        LVoid -> return NoValue
        LBool b -> compileAtomicLiteral "int" TBool (show $ if b then 1 else 0 :: Int)
        LString s ->
            do
                let s' = escapeString s
                var <- genVar (TPtr TByte)
                emitIndented $ "const char* " ++ compileValue var ++ " = " ++ s' ++ ";\n"
                return var
        LDecimal i ->
            do
                intType <- getLiteralDecimalType i
                typeName <- compileTypeName intType
                compileAtomicLiteral typeName intType (showDecimalLiteral intType i)
        LHexadecimal i ->
            do
                intType <- getLiteralHexType i
                typeName <- compileTypeName intType
                compileAtomicLiteral typeName intType (showHexLiteral intType i)
        LFloat f -> compileAtomicLiteral "double" TDouble (show f)

compileAtomicLiteral :: String -> Type -> String -> CompileMonad Value
compileAtomicLiteral ctype cygtype x = do
    var <- genVar cygtype
    emitIndented $ "const " ++ ctype ++ " " ++ compileValue var ++ " = " ++ x ++ ";\n"
    return var

escapeString :: String -> String
escapeString = show

parseCHeader :: String -> CompileMonad ()
parseCHeader header = do
    CompileState{compilerOptions = opts} <- get
    parseResult <- liftIO $ parse (includeDirs opts) header
    case parseResult of
        Just symbols ->
            let symbolsMap =
                    Map.fromList $
                        [ (name, symbol)
                        | symbol@(CSymbol{symbolName = name}) <- symbols
                        ]
             in modify $ \st -> st{compilerIncludes = compilerIncludes st ++ [symbolsMap]}
        _ -> error $ "Failed to parse \"" ++ header ++ "\""

getBlockType :: Block -> CompileMonad Type
getBlockType blk =
    case blk of
        [] -> return TVoid
        sts -> getStatementType $ last sts

getStatementType :: Statement -> CompileMonad Type
getStatementType st =
    case st of
        SReturn _ -> return TVoid
        SLet _ -> return TVoid
        SAssign _ _ -> return TVoid
        SIf _ t e ->
            do
                tt <- getBlockType t
                et <- getBlockType e
                return $ unify tt et
        SExpression expr -> getExpressionType expr

getExpressionType :: Expression -> CompileMonad Type
getExpressionType expr =
    case expr of
        EApply application ->
            do
                appTypes <- traverse getExpressionType application
                case appTypes of
                    (TFunction v _ _ : _) -> return $ fromJust $ getApplyType appTypes v
                    _ -> return $ fromJust $ getApplyType appTypes False
        ELiteral literal ->
            case literal of
                LVoid -> return TVoid
                LBool _ -> return TBool
                LString _ -> return (TPtr TByte)
                LDecimal x -> getLiteralDecimalType x
                LHexadecimal x -> getLiteralHexType x
                LFloat _ -> return TDouble
        ENamed name ->
            do
                resolved <- resolve name
                case resolved of
                    LocalValue _ (LocalVariable t _) -> return t
                    AnonymousValue _ t -> return t
                    BuiltInValue builtIn -> return $ builtInType builtIn
                    CSymbolValue csym -> return $ convertCToCygnet $ symbolType csym
                    CygnetSymbolValue sym -> return $ symbolGetType sym
                    _ -> error $ "Failed to resolve name \"" ++ name ++ "\""
        ETyped _ t -> return t
        ESizeOf _ -> getExpressionType (ENamed "size_t")

getLiteralDecimalType :: Integer -> CompileMonad Type
getLiteralDecimalType x =
    do
        limits <- gets (numericLimits . compilerOptions)
        if x >= intMin limits && x <= intMax limits
            then return TInt
            else
                if x >= longMin limits && x <= longMax limits
                    then return TLong
                    else
                        if x >= llongMin limits && x <= llongMax limits
                            then return TLLong
                            else error $ "Invalid decimal literal value: " ++ showDecimalLiteral TInt x

getLiteralHexType :: Integer -> CompileMonad Type
getLiteralHexType x =
    do
        limits <- gets (numericLimits . compilerOptions)
        if x >= intMin limits && x <= intMax limits
            then return TInt
            else
                if x >= 0 && x <= uintMax limits
                    then return TUInt
                    else
                        if x >= longMin limits && x <= longMax limits
                            then return TLong
                            else
                                if x >= 0 && x <= ulongMax limits
                                    then return TULong
                                    else
                                        if x >= llongMin limits && x <= llongMax limits
                                            then return TLLong
                                            else
                                                if x >= 0 && x <= ullongMax limits
                                                    then return TULLong
                                                    else error $ "Invalid hexadecimal literal value: " ++ showHexLiteral TInt x

showDecimalLiteral :: Type -> Integer -> String
showDecimalLiteral t x = show x ++ showIntLiteralSuffix t

showHexLiteral :: Type -> Integer -> String
showHexLiteral t x =
    if x < 0
        then "-" ++ showHexLiteral t (-x)
        else "0x" ++ showHex x (showIntLiteralSuffix t)

showIntLiteralSuffix :: Type -> String
showIntLiteralSuffix t =
    case t of
        TByte -> ""
        TUByte -> ""
        TShort -> ""
        TUShort -> ""
        TInt -> ""
        TUInt -> "u"
        TLong -> "l"
        TULong -> "ul"
        TLLong -> "ll"
        TULLong -> "ull"
        _ -> error $ "Invalid integer literal type: " ++ show t

unify :: Type -> Type -> Type
unify a _ = a

unifies :: Type -> Type -> Bool
unifies a b =
    case (a, b) of
        (TVar _, _) -> True
        (_, TVar _) -> True
        (TFunction _ a1 a2, TFunction _ b1 b2) -> unifies a1 b1 && unifies a2 b2
        (TProduct as, TProduct bs) -> length as == length bs && foldl' (\u (a', b') -> u && unifies a' b') True (zip as bs)
        _ -> a == b

getPartialType :: Int -> Type -> Maybe Type
getPartialType args f =
    case f of
        TFunction _ (TProduct []) r -> getPartialType args r
        TFunction v (TProduct [t]) r -> getPartialType args (TFunction v t r)
        _ ->
            if args == 0
                then Just f
                else case f of
                    TFunction v (TProduct (_ : ts)) r -> getPartialType (args - 1) (TFunction v (TProduct ts) r)
                    TFunction _ _ r -> getPartialType (args - 1) r
                    _ -> Nothing

getApplyType :: [Type] -> Variadic -> Maybe Type
getApplyType t variadic =
    case t of
        [] -> Nothing
        [t'] -> Just t'
        (f : x : xs) ->
            case f of
                TFunction _ (TProduct []) b -> getApplyType (b : xs) variadic
                TFunction v (TProduct [t']) b -> getApplyType (TFunction v t' b : x : xs) variadic
                TFunction v (TProduct (t' : ts)) b ->
                    if unifies x t'
                        then getApplyType (TFunction v (TProduct ts) b : xs) variadic
                        else Nothing
                TFunction _ a b ->
                    if unifies x a
                        then getApplyType (b : xs) variadic
                        else Nothing
                _ ->
                    if variadic
                        then getApplyType (x : xs) variadic
                        else Nothing

getFuncParams :: Type -> [String] -> [Value]
getFuncParams ftype = getFuncParams' (getFuncParamTypes ftype)
  where
    getFuncParams' ptypes params =
        case (ptypes, params) of
            ([], []) -> []
            ([], _) -> error "Too many parameters to function"
            (ts, []) -> zipWith AnonymousValue getAnonymousParameterNames ts
            (ptype : pts, param : ps) -> LocalValue param (LocalVariable ptype False) : getFuncParams' pts ps
    getFuncParamTypes ft =
        case ft of
            TFunction _ param _ ->
                case param of
                    TProduct ts -> ts
                    _ -> [param]
            _ -> []
