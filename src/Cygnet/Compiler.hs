{-# LANGUAGE ImportQualifiedPost #-}

module Cygnet.Compiler (CompilerOptions (..), compile) where

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
    { options :: CompilerOptions
    , includes :: [Map String CSymbol]
    , locals :: [Map String Type]
    , depth :: Int
    , nextVar :: Int
    , nextTypeVar :: Int
    , result :: CompileResult
    }

type CompileResult = Text

type CompileMonad a = StateT CompileState IO a

compile :: CompilerOptions -> Module -> IO CompileResult
compile opts unit = do
    let initialState =
            CompileState
                { options = opts
                , includes = []
                , locals = []
                , depth = 0
                , nextVar = 0
                , nextTypeVar = 0
                , result = Text.empty
                }
    ((), st) <-
        runStateT
            ( do
                traverse_ parseCHeader (moduleIncludes unit)
                let symbols = Map.elems $ moduleSymbols unit
                let fnSymbols = filter symbolIsFunction symbols
                fnDependencies <- Set.toList . Set.unions <$> traverse symbolDependencies fnSymbols
                fnResolvedDeps <- traverse resolve fnDependencies
                traverse_ compileResolvedFunctionSymbol (zip fnDependencies fnResolvedDeps)
                emit "\n"
                traverse_ (emit . compileFuncDecl) fnSymbols
                emit "\n"
                traverse_ compileFuncDef fnSymbols
            )
            initialState
    return $ result st

data ResolvedSymbol
    = ResolvedLocal String Type
    | ResolvedCSymbol CSymbol
    | ResolvedCygnetSymbol Symbol
    | AmbiguousSymbol [ResolvedSymbol]
    | UnresolvedSymbol
    deriving (Show)

resolve :: String -> CompileMonad ResolvedSymbol
resolve name = do
    local <- getLocal name
    case local of
        ResolvedLocal _ _ -> return local
        AmbiguousSymbol _ -> return local
        _ -> do
            st <- get
            let cLookups = map (Map.lookup name) (includes st)
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

symbolDependencies :: Symbol -> CompileMonad (Set String)
symbolDependencies symbol =
    case symbol of
        Symbol _ _ _ (Function body _ _) -> blockDependencies body

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
                ResolvedLocal _ _ -> return Set.empty
                _ -> return $ Set.singleton name
        ETyped expr' _ -> expressionDependencies expr'

emit :: String -> CompileMonad ()
emit text = modify $ \st -> st{result = Text.append (result st) (Text.pack text)}

emitIndented :: String -> CompileMonad ()
emitIndented text = modify $ \st -> st{result = Text.append (result st) (Text.pack (concat (replicate (depth st) "    ") ++ text))}

pushIndent :: CompileMonad ()
pushIndent = modify $ \st -> st{depth = depth st + 1}

popIndent :: CompileMonad ()
popIndent = modify $ \st -> st{depth = depth st - 1}

pushLocalBlock :: CompileMonad ()
pushLocalBlock = modify $ \st -> st{locals = Map.empty : locals st}

popLocalBlock :: CompileMonad ()
popLocalBlock = modify $ \st -> st{locals = tail (locals st)}

putLocal :: String -> Type -> CompileMonad ()
putLocal var varType = do
    (localBlock : parentBlocks) <- gets locals
    let local = Map.lookup var localBlock
    case local of
        Nothing -> modify $ \st -> st{locals = Map.insert var varType localBlock : parentBlocks}
        _ -> error $ "Duplicate variable name: " ++ var

getLocal :: String -> CompileMonad ResolvedSymbol
getLocal var = do
    st <- get
    let localLookups = mapMaybe (Map.lookup var) (locals st)
    let localResolved = map (ResolvedLocal var) (nub localLookups)
    case localResolved of
        local : _ -> return local
        [] -> return UnresolvedSymbol

genVar :: CompileMonad String
genVar = do
    varId <- gets nextVar
    modify $ \st -> st{nextVar = varId + 1}
    return $ "_cyg_var_" ++ show varId

genTypeVar :: CompileMonad String
genTypeVar = do
    typeVarId <- gets nextTypeVar
    modify $ \st -> st{nextTypeVar = typeVarId + 1}
    return $ "a" ++ show typeVarId

resetVars :: CompileMonad ()
resetVars = modify $ \st -> st{nextVar = 0, nextTypeVar = 0}

compileResolvedFunctionSymbol :: (String, ResolvedSymbol) -> CompileMonad ()
compileResolvedFunctionSymbol (name, symbol) =
    case symbol of
        ResolvedLocal var _ -> error $ "Expected top-level function, got local symbol: " ++ var
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
        CT_Int -> TInt
        CT_UInt -> undefined
        CT_Long -> undefined
        CT_ULong -> undefined
        CT_LLong -> undefined
        CT_ULLong -> undefined
        CT_Float -> undefined
        CT_Double -> undefined
        CT_LDouble -> undefined
        CT_Bool -> undefined
        CT_Function r ps v -> buildFuncType $ map convertCToCygnet $ ps ++ [r]
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
                ++ compileTypeName (getFuncRetType ftype)
                ++ " "
                ++ compileSymbolName symbol
                ++ "("
                ++ intercalate ", " (map compileFuncParam (getFuncParams ftype fparams))
                ++ ")"
        _ -> compileFuncProto $ Symbol saccess slinkage sname (Function fbody (TFunction TVoid ftype) ("" : fparams))

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
        TVar a -> "<type var " ++ a ++ ">"

compileFuncParam :: (Type, String) -> String
compileFuncParam (ptype, pname) =
    compileTypeName ptype
        ++ (if pname /= "" then " " ++ pname else "")

compileFuncDecl :: Symbol -> String
compileFuncDecl symbol = compileFuncProto symbol ++ ";\n"

compileFuncDef :: Symbol -> CompileMonad ()
compileFuncDef symbol@(Symbol _ _ _ (Function fbody _ _)) = do
    resetVars
    pushLocalBlock
    emit $ compileFuncProto symbol ++ "\n{\n"
    pushIndent
    traverse_ compileStatement fbody
    popIndent
    emit "}\n\n"
    popLocalBlock
  where
    compileStatement st =
        case st of
            SReturn expr -> compileReturn expr
            SLet assignments -> compileLet assignments
            SExpression expr -> compileExpression expr
    compileReturn expr =
        case expr of
            ELiteral LVoid -> emitIndented "return;\n" >> return ""
            _ -> compileExpression expr >>= \var -> emitIndented ("return " ++ var ++ ";\n") >> return ""
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
                            "" -> error $ "Assigning void to variable \"" ++ var ++ "\""
                            _ ->
                                do
                                    let varName = getName $ ResolvedLocal var assignmentType
                                    let varDecl = compileType assignmentType ++ " " ++ varName
                                    emitIndented $ varDecl ++ " = " ++ valVar ++ ";\n"
            assignmentTypes <- getAssignmentTypes assignments
            traverse_ compileAssignment (zip assignments assignmentTypes)
            return ""
    compileExpression expr =
        case expr of
            EApply (f : args) ->
                do
                    fVar <- compileExpression f
                    argVars <- traverse compileExpression args
                    resType <- getExpressionType expr
                    let fCall = fVar ++ "(" ++ intercalate ", " argVars ++ ");\n"
                    case resType of
                        TVoid -> emitIndented fCall >> return ""
                        _ ->
                            genVar >>= \resVar ->
                                let resDecl = compileType resType ++ " " ++ resVar
                                 in emitIndented (resDecl ++ " = " ++ fCall) >> return resVar
            EApply [] -> return ""
            ELiteral literal -> compileLiteral literal
            ENamed name ->
                do
                    resolved <- resolve name
                    case resolved of
                        AmbiguousSymbol _ -> error $ "Ambiguous symbol: \"" ++ name ++ "\""
                        UnresolvedSymbol -> error $ "Unresolved symbol: \"" ++ name ++ "\""
                        _ -> return $ getName resolved
            ETyped x _ -> compileExpression x
    compileLiteral literal =
        case literal of
            LVoid -> return ""
            LString s ->
                do
                    let s' = escapeString s
                    var <- genVar
                    emitIndented $ "const char* " ++ var ++ " = " ++ s' ++ ";\n"
                    return var
            LInteger i ->
                do
                    var <- genVar
                    emitIndented $ "const int " ++ var ++ " = " ++ show i ++ ";\n"
                    return var

getName :: ResolvedSymbol -> String
getName resolved =
    case resolved of
        ResolvedLocal var _ -> "_cyg_local_" ++ var
        ResolvedCSymbol csym -> symbolName csym
        ResolvedCygnetSymbol (Symbol _ _ name _) -> name
        AmbiguousSymbol _ -> error "Ambiguous symbol"
        UnresolvedSymbol -> error "Unresolved symbol"

compileType :: Type -> String
compileType t =
    case t of
        TVoid -> "void"
        TString -> "const char*"
        TInt -> "int"
        TFunction a b -> "<func type>"
        TVar var -> var

escapeString :: String -> String
escapeString = show

parseCHeader :: String -> CompileMonad ()
parseCHeader header = do
    CompileState{options = opts} <- get
    parseResult <- liftIO $ parse (includeDirs opts) header
    case parseResult of
        Just symbols ->
            let symbolsMap = Map.fromList $ [(name, symbol) | symbol@(CSymbol{symbolName = name}) <- symbols]
             in modify $ \st -> st{includes = includes st ++ [symbolsMap]}
        _ -> error $ "Failed to parse \"" ++ header ++ "\""

getStatementType :: Statement -> CompileMonad Type
getStatementType st =
    case st of
        SReturn _ -> return TVoid
        SLet _ -> return TVoid
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
                LString _ -> return TString
                LInteger _ -> return TInt
        ENamed name ->
            do
                resolved <- resolve name
                case resolved of
                    ResolvedLocal _ t -> return t
                    ResolvedCSymbol csym ->
                        case symbolType csym of
                            CT_Function _ _ variadic ->
                                if variadic
                                    then error $ "The FFI can't handle variadic functions: \"" ++ symbolName csym ++ "\""
                                    else return $ convertCToCygnet $ symbolType csym
                            _ -> return $ convertCToCygnet $ symbolType csym
                    ResolvedCygnetSymbol (Symbol _ _ _ (Function _ t _)) -> return t
                    _ -> error $ "Failed to resolve name \"" ++ name ++ "\""
        ETyped _ t -> return t

unifies :: Type -> Type -> Bool
unifies a b =
    case (a, b) of
        (TVar _, _) -> True
        (_, TVar _) -> True
        (TFunction a1 a2, TFunction b1 b2) -> unifies a1 b1 && unifies a2 b2
        _ -> a == b

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

getFuncRetType :: Type -> Type
getFuncRetType t =
    case t of
        TFunction _ retType -> getFuncRetType retType
        _ -> t

getFuncParams :: Type -> [String] -> [(Type, String)]
getFuncParams ftype = zip (getParamTypes ftype)
  where
    getParamTypes ft =
        case ft of
            TFunction param ret -> param : getParamTypes ret
            _ -> []
