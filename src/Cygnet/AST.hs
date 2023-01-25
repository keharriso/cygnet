module Cygnet.AST (
    Module (..),
    Symbol (..),
    Access (..),
    Linkage (..),
    TopLevel (..),
    Block,
    Statement (..),
    Expression (..),
    Literal (..),
    Type (..),
    symbolIsFunction,
    getFuncRetType,
    getFuncParams,
) where

import Data.Map (Map)

data Module = Module
    { moduleName :: String
    , moduleIncludes :: [String]
    , moduleSymbols :: Map String Symbol
    }
    deriving (Eq, Show)

data Symbol = Symbol Access Linkage String TopLevel
    deriving (Eq, Show)

data Access
    = Private
    | Public
    deriving (Eq, Show)

data Linkage
    = Cygnet
    | C
    deriving (Eq, Show)

data TopLevel
    = Function Block Type [ParameterName]
    deriving (Eq, Show)

type ParameterName = String

symbolIsFunction :: Symbol -> Bool
symbolIsFunction (Symbol _ _ _ (Function{})) = True

type Block = [Statement]

data Statement
    = SReturn Expression
    | SLet String [String] Statement
    | SExpression Expression
    deriving (Eq, Show)

data Expression
    = EApply [Expression]
    | ELiteral Literal
    | ENamed String
    | ETyped Expression Type
    deriving (Eq, Show)

data Literal
    = LVoid
    | LString String
    | LInteger Integer
    deriving (Eq, Show)

data Type
    = TVoid
    | TString
    | TInt
    | TFunction Type Type
    | TVar String
    deriving (Eq, Show)

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
