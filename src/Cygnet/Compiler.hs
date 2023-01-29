{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}

module Cygnet.Compiler (CompilerOptions (..), compile) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, get, gets, modify, runStateT)

import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (intercalate, nub, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

import Ocelot

import Cygnet.AST

data CompilerOptions = CompilerOptions {includeDirs :: [String]}
    deriving (Show)

data CompileState = CompileState
    { compilerOptions :: CompilerOptions
    , compilerImports :: [Import]
    , compilerIncludes :: [Map String CSymbol]
    , compilerLocals :: [Map String Type]
    , compilerDepth :: Int
    , compilerNextVar :: Int
    , compilerNextTypeVar :: Int
    , compilerResult :: CompileResult
    }

type Import = Module

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
                , compilerDepth = 0
                , compilerNextVar = 0
                , compilerNextTypeVar = 0
                , compilerResult = Text.empty
                }
    ((), st) <-
        runStateT
            ( do
                traverse_ parseCHeader (moduleIncludes unit)
                let symbols = Map.elems $ moduleSymbols unit
                let fnSymbols = filter symbolIsFunction symbols
                fnDependencies <- Set.toList . Set.unions <$> traverse symbolDependencies fnSymbols
                fnResolvedDeps <- traverse resolve fnDependencies
                let isExternal dep =
                        case dep of
                            CygnetSymbolValue sym -> sym `notElem` moduleSymbols unit
                            _ -> True
                let externalDeps = filter isExternal fnResolvedDeps
                traverse_ compileResolvedFunctionSymbol externalDeps
                emit "\n"
                traverse_ (emit . compileFuncDecl) fnSymbols
                emit "\n"
                traverse_ compileFuncDef fnSymbols
            )
            initialState
    return $ compilerResult st

data Value
    = TemporaryValue Int Type
    | LocalValue String Type
    | PartialValue Value [Value]
    | BuiltInValue BuiltIn
    | CSymbolValue CSymbol
    | CygnetSymbolValue Symbol
    | AmbiguousValue String [Value]
    | UnresolvedValue String
    | NoValue
    deriving (Eq, Show)

compileValue :: Value -> String
compileValue value = compileValues [value]

compileValues :: [Value] -> String
compileValues values =
    case values of
        (BuiltInValue builtIn : args) -> "(" ++ builtInCompile builtIn args ++ ")"
        [TemporaryValue varId _] -> "_cyg_temp_" ++ show varId
        [LocalValue var _] -> "_cyg_local_" ++ mangle var
        [PartialValue _ _] -> undefined
        [CSymbolValue csym] -> symbolName csym
        [CygnetSymbolValue (Symbol _ linkage name _)] ->
            case linkage of
                Cygnet -> "_cyg_tl_" ++ mangle name
                C -> name
        [AmbiguousValue name _] -> error $ "Ambiguous symbol: \"" ++ name ++ "\""
        [UnresolvedValue name] -> error $ "Unresolved symbol: \"" ++ name ++ "\""
        [NoValue] -> error "No value"
        (f : args) ->
            if getArity f == length args
                then compileCompleteApply f args
                else compilePartialApply f args
        _ -> undefined
  where
    mangle = id
    compileCompleteApply f args =
        let compiledArgs = map compileValue args
         in compileValue f ++ "(" ++ intercalate ", " compiledArgs ++ ")"
    compilePartialApply f args = undefined

getType :: Value -> Type
getType v =
    case v of
        TemporaryValue _ t -> t
        LocalValue _ t -> t
        PartialValue f args ->
            let types = map getType (f : args)
             in fromJust $ getApplyType types
        BuiltInValue (BuiltIn{builtInType = t}) -> t
        CSymbolValue csym -> convertCToCygnet $ symbolType csym
        CygnetSymbolValue sym -> symbolGetType sym
        AmbiguousValue name _ -> error $ "Ambiguous symbol: \"" ++ name ++ "\""
        UnresolvedValue name -> error $ "Unresolved symbol: \"" ++ name ++ "\""
        NoValue -> error "No value"

getArity :: Value -> Int
getArity v =
    case v of
        TemporaryValue _ t -> typeArity t
        LocalValue _ t -> typeArity t
        PartialValue f args -> getArity f - length args
        BuiltInValue builtIn -> builtInArity builtIn
        CSymbolValue (CSymbol{symbolType = CT_Function _ params _}) -> length params
        CygnetSymbolValue (Symbol _ _ _ (Function _ t _)) -> typeArity t
        _ -> 0
  where
    typeArity t =
        case t of
            TFunction TVoid ret -> typeArity ret
            TFunction _ ret -> 1 + typeArity ret
            _ -> 0

data BuiltIn = BuiltIn
    { builtInName :: String
    , builtInType :: Type
    , builtInArity :: Int
    , builtInCompile :: [Value] -> String
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
        [ binaryBuiltIn "+" TNumber TNumber TNumber $ \a b -> a ++ " + " ++ b
        , binaryBuiltIn "-" TNumber TNumber TNumber $ \a b -> a ++ " - " ++ b
        , binaryBuiltIn "*" TNumber TNumber TNumber $ \a b -> a ++ " * " ++ b
        , binaryBuiltIn "/" TNumber TNumber TNumber $ \a b -> a ++ " / " ++ b
        ]
  where
    binaryBuiltIn name a b c def =
        let compileBinary values =
                case map compileValue values of
                    [x, y] -> def x y
                    _ -> error $ "Binary operator applied to " ++ show (length values) ++ " arguments"
         in (name, BuiltIn
                    { builtInName = name
                    , builtInType = TFunction a (TFunction b c)
                    , builtInArity = 2
                    , builtInCompile = compileBinary
                    })

resolve :: String -> CompileMonad Value
resolve name = do
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
                    let cygResolved = map CygnetSymbolValue $
                            sortOn (\(Symbol _ _ name' _) -> name') . nub $ catMaybes cygLookups
                    let cLookups = map (Map.lookup name) (compilerIncludes st)
                    let cResolved = map CSymbolValue $
                            sortOn symbolName . nub $ catMaybes cLookups
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
        Symbol _ _ _ (Function body ft ps) ->
            do
                pushLocalBlock
                let params = getFuncParams ft ps
                traverse_ (\(t, p) -> putLocal p t) params
                deps <- blockDependencies body
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
            traverse_ (\(Assignment f _ _) -> putLocal f TVoid) assignments
            Set.unions <$> traverse assignmentDependencies assignments
        SIf cond tBranch fBranch -> do
            condDeps <- expressionDependencies cond
            tBranchDeps <- blockDependencies tBranch
            fBranchDeps <- blockDependencies fBranch
            return $ Set.unions [condDeps, tBranchDeps, fBranchDeps]
  where
    assignmentDependencies (Assignment _ args st') =
        do
            pushLocalBlock
            traverse_ (`putLocal` TVoid) args
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
        ETyped expr' _ -> expressionDependencies expr'

emit :: String -> CompileMonad ()
emit text = modify $ \st -> st{compilerResult = Text.append (compilerResult st) (Text.pack text)}

emitIndented :: String -> CompileMonad ()
emitIndented text = modify $ \st -> st{compilerResult =
    Text.append (compilerResult st) (Text.pack (concat (replicate (compilerDepth st) "    ") ++ text))}

pushIndent :: CompileMonad ()
pushIndent = modify $ \st -> st{compilerDepth = compilerDepth st + 1}

popIndent :: CompileMonad ()
popIndent = modify $ \st -> st{compilerDepth = compilerDepth st - 1}

pushLocalBlock :: CompileMonad ()
pushLocalBlock = modify $ \st -> st{compilerLocals = Map.empty : compilerLocals st}

popLocalBlock :: CompileMonad ()
popLocalBlock = modify $ \st -> st{compilerLocals = tail (compilerLocals st)}

putLocal :: String -> Type -> CompileMonad ()
putLocal var varType = do
    (localBlock : parentBlocks) <- gets compilerLocals
    let local = Map.lookup var localBlock
    case local of
        Nothing -> modify $ \st -> st{compilerLocals = Map.insert var varType localBlock : parentBlocks}
        _ -> error $ "Duplicate variable name: " ++ var

setLocal :: String -> Type -> CompileMonad ()
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

compileResolvedFunctionSymbol :: Value -> CompileMonad ()
compileResolvedFunctionSymbol value =
    case value of
        TemporaryValue _ _ -> error "Expected top-level function, got temporary variable"
        LocalValue var _ -> error $ "Expected top-level function, got local symbol: \"" ++ var ++ "\""
        PartialValue _ _ -> error "Expected top-level function, got partial application"
        BuiltInValue _ -> return ()
        CSymbolValue csym -> compileCFuncDecl csym >>= emit
        CygnetSymbolValue cygsym -> emit $ compileFuncDecl cygsym
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
            case (t, n) of
                (CT_Char, 1) -> TString
                _ -> undefined
        CT_Array t n -> undefined
        CT_Char -> undefined
        CT_UChar -> undefined
        CT_Short -> undefined
        CT_UShort -> undefined
        CT_Int -> undefined
        CT_UInt -> undefined
        CT_Long -> undefined
        CT_ULong -> undefined
        CT_LLong -> undefined
        CT_ULLong -> undefined
        CT_Float -> undefined
        CT_Double -> TNumber
        CT_LDouble -> undefined
        CT_Bool -> TBool
        CT_Function r ps v ->
            if v
                then undefined
                else
                    buildFuncType $
                        map convertCToCygnet $
                            if null ps
                                then [CT_Void, r]
                                else ps ++ [r]
        CT_Struct fs -> undefined
        CT_Union fs -> undefined
        CT_Enum fs -> undefined
        CT_Named n -> undefined
  where
    buildFuncType curriedTypes =
        case curriedTypes of
            [] -> TVoid
            [t] -> t
            (t : ts) -> TFunction t (buildFuncType ts)

compileFuncProto :: Symbol -> String
compileFuncProto symbol@(Symbol saccess slinkage sname (Function fbody ftype fparams)) =
    case ftype of
        TFunction _ _ ->
            compileAccess saccess
                ++ compileTypeName (fromJust $ getPartialType (length fparams) ftype)
                ++ " "
                ++ compileSymbolName symbol
                ++ "("
                ++ intercalate ", " (compileFuncParams ftype fparams)
                ++ ")"
        _ -> compileFuncProto $ Symbol saccess slinkage sname
                (Function fbody (TFunction TVoid ftype) ("" : fparams))

compileAccess :: Access -> String
compileAccess access = case access of
    Private -> "static "
    Public -> ""

compileSymbolName :: Symbol -> String
compileSymbolName sym = compileValue $ CygnetSymbolValue sym

compileTypeName :: Type -> String
compileTypeName t =
    case t of
        TVoid -> "void"
        TBool -> "int"
        TString -> "char*"
        TNumber -> "double"
        TFunction _ _ -> "<fn type>"
        TVar a -> "<type var " ++ a ++ ">"

compileFuncParam :: (Type, String) -> String
compileFuncParam (ptype, pname) =
    let pval = LocalValue pname ptype
     in compileTypeName ptype ++ (if pname /= "" then " " ++ compileValue pval else "")

compileFuncParams :: Type -> [String] -> [String]
compileFuncParams ftype params = map compileFuncParam (getFuncParams ftype params)

compileFuncDecl :: Symbol -> String
compileFuncDecl symbol = compileFuncProto symbol ++ ";\n"

compileFuncDef :: Symbol -> CompileMonad ()
compileFuncDef symbol@(Symbol _ _ _ (Function fbody ftype params)) = do
        beginFuncDef
        let retType = fromJust $ getPartialType (length params) ftype
        value <- compileBlock fbody
        if retType /= TVoid && value /= NoValue
            then emitIndented ("return " ++ compileValue value ++ ";\n") >> endFuncDef
            else endFuncDef
    where
        beginFuncDef = do
            resetVars
            pushLocalBlock
            emit $ compileFuncProto symbol ++ "\n{\n"
            traverse_ (\(t, p) -> putLocal p t) (getFuncParams ftype params)
            pushIndent
        endFuncDef = do
            popIndent
            emit "}\n\n"
            popLocalBlock

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
        SIf cond t e -> compileIf cond t e
        SExpression expr -> compileExpression expr

compileReturn :: Expression -> CompileMonad Value
compileReturn expr =
    case expr of
        ELiteral LVoid -> emitIndented "return;\n" >> return NoValue
        _ -> compileExpression expr >>= \var ->
            emitIndented ("return " ++ compileValue var ++ ";\n") >> return NoValue

compileLet :: [Assignment] -> CompileMonad Value
compileLet assignments =
    do
        let quantifyTypeVars vars =
                do
                    tvars <- traverse (const genTypeVar) vars
                    traverse_ (\(var, tvar) -> putLocal var (TVar tvar)) (zip vars tvars)
        let getAssignmentType (Assignment _ args st) =
                do
                    pushLocalBlock
                    quantifyTypeVars args
                    t <- getStatementType st
                    popLocalBlock
                    return t
        let getAssignmentTypes as =
                do
                    let vars = Set.fromList $ map (\(Assignment var _ _) -> var) as
                    quantifyTypeVars $ Set.toList vars
                    traverse getAssignmentType as
        let compileAssignment (Assignment var args st, assignmentType) =
                do
                    pushLocalBlock
                    quantifyTypeVars args
                    valVar <- compileStatement st
                    popLocalBlock
                    case valVar of
                        NoValue -> error $ "Assigning void to variable \"" ++ var ++ "\""
                        _ ->
                            do
                                setLocal var assignmentType
                                let varName = compileValue $ LocalValue var assignmentType
                                let varDecl = compileType assignmentType ++ " " ++ varName
                                emitIndented $ varDecl ++ " = " ++ compileValue valVar ++ ";\n"
        assignmentTypes <- getAssignmentTypes assignments
        traverse_ compileAssignment (zip assignments assignmentTypes)
        return NoValue

compileIf :: Expression -> Block -> Block -> CompileMonad Value
compileIf cond t e = do
    resultType <- getStatementType $ SIf cond t e
    resultVar <-
        case resultType of
            TVoid -> return NoValue
            _ ->
                do
                    var <- genVar resultType
                    emitIndented $ compileType resultType ++ " " ++ compileValue var ++ ";\n"
                    return var
    condVal <- compileExpression cond
    emitIndented $ "if (" ++ compileValue condVal ++ ")\n"
    emitIndented "{\n"
    pushIndent
    thenResult <- compileBlock t
    when (resultType /= TVoid && resultVar /= NoValue)
        (emitIndented $ compileValue resultVar ++ " = " ++ compileValue thenResult ++ ";\n")
    unless (null e) $ do
        popIndent
        emitIndented "}\n"
        emitIndented "else\n"
        emitIndented "{\n"
        pushIndent
        elseResult <- compileBlock e
        when (resultType /= TVoid && resultVar /= NoValue)
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
        ETyped x _ -> compileExpression x

compileApply :: [Expression] -> CompileMonad Value
compileApply exprs = do
    applyValues <- traverse compileExpression exprs
    resultType <- getExpressionType $ EApply exprs
    (left, result) <- case resultType of
        TVoid -> return ("", NoValue)
        _ -> genVar resultType >>= \var ->
            return (compileType resultType ++ " " ++ compileValue var ++ " = ", var)
    let right = compileValues applyValues
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
                var <- genVar TString
                emitIndented $ "const char* " ++ compileValue var ++ " = " ++ s' ++ ";\n"
                return var
        LInteger i -> compileAtomicLiteral "int" TNumber (show i)
        LFloat f -> compileAtomicLiteral "double" TNumber (show f)

compileAtomicLiteral :: String -> Type -> String -> CompileMonad Value
compileAtomicLiteral ctype cygtype x = do
    var <- genVar cygtype
    emitIndented $ "const " ++ ctype ++ " " ++ compileValue var ++ " = " ++ x ++ ";\n"
    return var

compileType :: Type -> String
compileType t =
    case t of
        TVoid -> "void"
        TBool -> "int"
        TString -> "const char*"
        TNumber -> "double"
        TFunction a b -> "<func type>"
        TVar var -> var

escapeString :: String -> String
escapeString = show

parseCHeader :: String -> CompileMonad ()
parseCHeader header = do
    CompileState{compilerOptions = opts} <- get
    parseResult <- liftIO $ parse (includeDirs opts) header
    case parseResult of
        Just symbols ->
            let symbolsMap = Map.fromList $ [(name, symbol) |
                    symbol@(CSymbol{symbolName = name}) <- symbols]
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
                return $ fromJust $ getApplyType appTypes
        ELiteral literal ->
            case literal of
                LVoid -> return TVoid
                LBool _ -> return TBool
                LString _ -> return TString
                LInteger _ -> return TNumber
                LFloat _ -> return TNumber
        ENamed name ->
            do
                resolved <- resolve name
                case resolved of
                    LocalValue _ t -> return t
                    BuiltInValue builtIn -> return $ builtInType builtIn
                    CSymbolValue csym ->
                        case symbolType csym of
                            CT_Function _ _ variadic ->
                                if variadic
                                    then error $ "The FFI can't handle variadic functions: \"" ++
                                                    symbolName csym ++ "\""
                                    else return $ convertCToCygnet $ symbolType csym
                            _ -> return $ convertCToCygnet $ symbolType csym
                    CygnetSymbolValue sym -> return $ symbolGetType sym
                    _ -> error $ "Failed to resolve name \"" ++ name ++ "\""
        ETyped _ t -> return t

unify :: Type -> Type -> Type
unify a b = a

unifies :: Type -> Type -> Bool
unifies a b =
    case (a, b) of
        (TVar _, _) -> True
        (_, TVar _) -> True
        (TFunction a1 a2, TFunction b1 b2) -> unifies a1 b1 && unifies a2 b2
        _ -> a == b

getPartialType :: Int -> Type -> Maybe Type
getPartialType args f =
    if args == 0
        then Just f
        else
            case f of
                TFunction _ r -> getPartialType (args - 1) r
                _ -> Nothing

getApplyType :: [Type] -> Maybe Type
getApplyType t =
    case t of
        [] -> Nothing
        [t'] -> Just t'
        (f : x : xs) ->
            case f of
                TFunction a b ->
                    if unifies x a
                        then getApplyType (b : xs)
                        else Nothing
                _ -> Nothing

getFuncParams :: Type -> [String] -> [(Type, String)]
getFuncParams ftype = zip (getParamTypes ftype)
  where
    getParamTypes ft =
        case ft of
            TFunction param ret -> param : getParamTypes ret
            _ -> []
