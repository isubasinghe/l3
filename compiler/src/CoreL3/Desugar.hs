{-# LANGUAGE OverloadedStrings #-}

module CoreL3.Desugar where

import Control.Monad.State
import CoreL3.AST
import qualified Data.DList as L
import qualified Data.Map.Strict as M
import Data.Text (Text)

data Stack a
  = Nil
  | Cons a (Stack a)

type DesugarState = State (Stack Int)

class Desugar a b where
  desugar :: a -> DesugarState b

pushNewFrame :: DesugarState ()
pushNewFrame = undefined

popFrame :: DesugarState (Stack Int)
popFrame = undefined

freshVariable :: DesugarState Text
freshVariable = undefined

instance Desugar Program [Expr] where
  desugar (Program p e) = desugar (p, e)
