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
    symbolGetType,
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
    | TFunction Type Type Variadic
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
        Function _ t _ -> t
