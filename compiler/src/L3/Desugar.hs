{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module L3.Desugar where

import Control.Monad.Except
import Control.Monad.State.Strict
import L3.AST
-- import qualified Data.DList as L
-- import Data.List (isPrefixOf)
-- import qualified Data.Map.Strict as M
import Data.Text (Text)

data DesugarError
  = ReservedVariablePattern (Maybe Text)
  | ExpectedExpression
  | InvalidState Text
  deriving (Show, Eq)

type DesugarState = ExceptT DesugarError (State Int)

class Desugar a b where
  desugar :: a -> DesugarState b

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
  desugar (EBegin (Begin ([]))) = throwError $ InvalidState "Begin cannot contain an empty list"
  desugar (EBegin (Begin ([e]))) = desugar e
  desugar (EBegin (Begin (e : es))) = do
    v <- freshVariable
    e' <- desugar e
    es' <- desugar (EBegin (Begin es))
    pure (ELet (Let [(v, e')] [es']))
  -- 
  desugar (ELet (Let bs es)) = do
    bes <- desugar bs
    es' <- desugar (EBegin (Begin es))
    pure (ELet (Let bes [es']))
  desugar (ELet (LetStar bs es)) = undefined
  desugar a = pure a

instance Desugar [(Ident, Expr)] [(Ident, Expr)] where
  desugar bs = do
    let (ns, es) = unzip bs
    es' <- mapM desugar es
    pure $ zip ns es'

instance Desugar Fun Fun where
  desugar _ = undefined
