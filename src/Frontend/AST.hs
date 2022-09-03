module Frontend.AST where

import Data.Text (Text)

type Ident = Text

data Definition
  = Def
  | DefRec
  deriving (Show, Eq)

data Fun = Fun
  deriving (Show, Eq)

data Let = Let
  deriving (Show, Eq)

data Rec = Rec
  deriving (Show, Eq)

data Begin = Begin
  deriving (Show, Eq)

data If = If
  deriving (Show, Eq)

data Cond = Cond
  deriving (Show, Eq)

data And = And
  deriving (Show, Eq)

data Or = Or
  deriving (Show, Eq)

data Not = Not Expr
  deriving (Show, Eq)

data App = App [Expr]
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
