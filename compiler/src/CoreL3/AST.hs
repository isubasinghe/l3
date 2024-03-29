module CoreL3.AST where

import Data.Text (Text)

type Ident = Text

data Fun = Fun [Ident] [Expr]
  deriving (Show, Eq)

data Let
  = Let [(Ident, Expr)] [Expr]
  | LetStar [(Ident, Expr)] [Expr]
  | LetRec [(Ident, Fun)] [Expr]
  deriving (Show, Eq)

data If = If Expr Expr (Maybe Expr)
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

data Prim = Prim PrimOp [Expr]
  deriving (Show, Eq)

data Expr
  = EFun Fun
  | ELet Let
  | EIf If
  | EApp App
  | EPrim Prim
  | EIdent Ident
  | ENum Int
  | EStr Text
  | EChr Char
  | EBool Bool
  | EUnit
  deriving (Show, Eq)
