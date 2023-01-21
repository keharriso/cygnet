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
    deriving (Show)

data Symbol = Symbol Access Linkage String TopLevel
    deriving (Show)

data Access
    = Private
    | Public
    deriving (Show)

data Linkage
    = Cygnet
    | C
    deriving (Show)

data TopLevel
    = Function Block Type [ParameterName]
    deriving (Show)

type ParameterName = String

symbolIsFunction :: Symbol -> Bool
symbolIsFunction (Symbol _ _ _ (Function{})) = True

type Block = [Statement]

data Statement
    = SReturn Expression
    | SExpression Expression
    deriving (Show)

data Expression
    = EApply [Expression]
    | ELiteral Literal
    | ENamed String
    deriving (Show)

data Literal
    = LVoid
    | LString String
    | LInteger Integer
    deriving (Show)

data Type
    = TVoid
    | TString
    | TInt
    | TFunction Type Type
    deriving (Show)

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
