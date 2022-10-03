{-# LANGUAGE OverloadedStrings #-}

module CoreL3.Desugar where

import Control.Monad.State
import CoreL3.AST
import qualified Data.DList as DList
import qualified Data.Map.Strict as M
import Data.Text

data Stack a
  = Nil
  | Cons a (Stack a)

type DesugarState = State (Stack Int)

freshVariable :: DesugarState Text
freshVariable = do
  s <- get
  undefined

desugarProgram :: Program -> [Expr]
desugarProgram p = undefined
