{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module CoreL3.Desugar where

import Control.Monad.Except
import Control.Monad.State.Strict
import CoreL3.AST
-- import qualified Data.DList as L
-- import Data.List (isPrefixOf)
-- import qualified Data.Map.Strict as M
import Data.Text (Text)

data Stack a
  = Nil
  | Cons a (Stack a)

data DesugarError = ReservedVariablePattern (Maybe Text)
  | ExpectedExpression
  deriving (Show, Eq)

type DesugarState = ExceptT DesugarError (State (Stack Int))

class Desugar a b where
  desugar :: a -> DesugarState b

pushNewFrame :: DesugarState ()
pushNewFrame = undefined

popFrame :: DesugarState (Stack Int)
popFrame = undefined

freshVariable :: DesugarState Text
freshVariable = undefined

instance Desugar Program Expr where
  desugar (Program p e) = desugar (p ++ [PExpr e])

instance Desugar ProgramItems Expr where
  desugar [] = throwError ExpectedExpression
  desugar [e] = case e of
    (PExpr pe) -> pure pe
    _ -> throwError ExpectedExpression
  desugar (p : ps) = case p of
    (PDefinition (Def n e)) -> do
      e' <- desugar e
      ps' <- desugar ps
      let ret = ELet $ Let [(n, e')] [ps']
      pure ret
    (PDefinition (DefRec n e)) -> do
      e' <- desugar e
      ps' <- desugar ps
      pure (ELet $ LetRec [(n, e')] [ps'])
    (PExpr e) -> do
      e' <- desugar e
      pure $ EBegin (Begin [e'])

instance Desugar Expr Expr where
  desugar (EBegin (Begin (e : es))) = do
    v <- freshVariable
    e' <- desugar (EBegin (Begin es))
    pure (ELet (Let [] [e']))

instance Desugar Fun Fun where
  desugar e = undefined
