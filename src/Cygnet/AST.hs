module Cygnet.AST (
    Module (..),
    Symbol (..),
    Access (..),
    Linkage (..),
    TopLevel (..),
    Block,
    Statement (..),
    Assignment (..),
    Expression (..),
    Literal (..),
    Type (..),
    Kind (..),
    Mutable,
    Variadic,
    symbolIsFunction,
    symbolIsConstant,
    symbolGetType,
    isConst,
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
    = TLFunction Type [ParameterName] Block
    | TLConstant Type Block
    deriving (Eq, Show)

type ParameterName = String

symbolIsFunction :: Symbol -> Bool
symbolIsFunction (Symbol _ _ _ tl) =
    case tl of
        TLFunction{} -> True
        _ -> False

symbolIsConstant :: Symbol -> Bool
symbolIsConstant (Symbol _ _ _ tl) =
    case tl of
        TLConstant{} -> True
        _ -> False

type Block = [Statement]

data Statement
    = SReturn Expression
    | SLet [Assignment]
    | SAssign String Statement
    | SIf Expression Block Block
    | SExpression Expression
    deriving (Eq, Show)

data Assignment = Assignment Mutable String [String] Statement
    deriving (Eq, Show)

type Mutable = Bool

data Expression
    = EApply [Expression]
    | ELiteral Literal
    | ENamed String
    | ETyped Expression Type
    | ESizeOf Type
    deriving (Eq, Show)

data Literal
    = LVoid
    | LBool Bool
    | LString String
    | LInteger Integer
    | LFloat Double
    deriving (Eq, Show)

data Type
    = TVoid
    | TPtr Type
    | TByte
    | TUByte
    | TShort
    | TUShort
    | TInt
    | TUInt
    | TLong
    | TULong
    | TLLong
    | TULLong
    | TFloat
    | TDouble
    | TLDouble
    | TBool
    | TProduct [Type]
    | TFunction Variadic Type Type
    | TNamed String
    | TConstructor Type Type
    | TVar String
    deriving (Eq, Show)

type Variadic = Bool

data Kind
    = KType
    | KConstraint
    | KArrow Kind Kind
    deriving (Eq, Show)

symbolGetType :: Symbol -> Type
symbolGetType (Symbol _ _ _ tl) =
    case tl of
        TLFunction t _ _ -> t
        TLConstant t _ -> t

isConst :: Block -> Bool
isConst blk =
    case blk of
        [] -> True
        [st] -> isStatementConst st
        _ -> False
  where
    isStatementConst st =
        case st of
            SExpression expr -> isExprConst expr
            _ -> False
    isExprConst expr =
        case expr of
            ELiteral lit ->
                case lit of
                    LBool _ -> True
                    LInteger _ -> True
                    LFloat _ -> True
                    _ -> False
            ESizeOf _ -> True
            _ -> False
