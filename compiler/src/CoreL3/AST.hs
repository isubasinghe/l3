module CoreL3.AST where

import Data.Text (Text)

type Ident = Text

data Program = Program ProgramItems Expr
  deriving (Show, Eq)

data ProgramItem
  = PDefinition Definition
  | PExpr Expr
  deriving (Show, Eq)

type ProgramItems = [ProgramItem]

data Definition
  = Def Ident Expr
  | DefRec Ident Fun
  deriving (Show, Eq)

data Fun = Fun [Ident] [Expr]
  deriving (Show, Eq)

data Let
  = Let [(Ident, Expr)] [Expr]
  | LetStar [(Ident, Expr)] [Expr]
  | LetRec [(Ident, Fun)] [Expr]
  deriving (Show, Eq)

data Rec = Rec Ident [(Ident, Expr)] [Expr]
  deriving (Show, Eq)

data Begin = Begin [Expr]
  deriving (Show, Eq)

data If = If Expr Expr (Maybe Expr)
  deriving (Show, Eq)

data Cond = Cond [(Expr, [Expr])] -- > 0
  deriving (Show, Eq)

data And = And [Expr]
  deriving (Show, Eq)

data Or = Or [Expr]
  deriving (Show, Eq)

data Not = Not Expr
  deriving (Show, Eq)

data App = App [Expr]
  deriving (Show, Eq)

data PrimOp
  = Plus
  | Minus
  | Xor
  | Shl
  | Shr
  | Div
  | Mod
  | Less
  | Leq
  | Eq
  | Id
  deriving (Show, Eq)

data Expr
  = EFun Fun
  | ELet Let
  | ERec Rec
  | EBegin Begin
  | EIf If
  | ECond Cond
  | EAnd And
  | EOr Or
  | ENot Not
  | EApp App
  | EIdent Ident
  | ENum Int
  | EStr Text
  | EChr Char
  | EBool Bool
  | EUnit
  deriving (Show, Eq)
