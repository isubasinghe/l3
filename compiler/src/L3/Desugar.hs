{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module L3.Desugar where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified CoreL3.AST as CL3
-- import qualified Data.DList as L
-- import Data.List (isPrefixOf)
-- import qualified Data.Map.Strict as M
import Data.Text (Text)
import L3.AST

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

instance Desugar Program CL3.Expr where
  desugar (Program p e) = desugar (p, e)

instance Desugar ([Expr], Expr) CL3.Expr where
  desugar ([], e) = desugar e
  desugar ([p], e) = undefined
  desugar ((p : ps), e) = case p of
    EDef (Def dn de) -> desugarDef dn de CL3.Let
    EDef (DefRec dn de) -> desugarDef dn de CL3.LetRec
    other -> undefined
    where
      desugarDef dn de fn = do 
        de' <- desugar de 
        ps' <- desugar (ps,e)
        pure (CL3.ELet (fn [(dn, de')] [ps']))


instance Desugar Expr CL3.Expr where
  desugar (EBegin (Begin ([]))) = throwError $ InvalidState "Begin cannot contain an empty list"
  desugar (EBegin (Begin ([e]))) = desugar e
  desugar (EBegin (Begin (e : es))) = do
    v <- freshVariable
    e' <- desugar e
    es' <- desugar (EBegin (Begin es))
    pure (CL3.ELet (CL3.Let [(v, e')] [es']))
  --
  desugar (ELet (Let bs es)) = do
    bes <- desugar bs
    es' <- desugar (EBegin (Begin es))
    pure (CL3.ELet (CL3.Let bes [es']))
  desugar (ELet (LetStar bs es)) = undefined
  desugar a = undefined

instance Desugar Expr Expr where
  desugar a = undefined

instance Desugar [(Ident, Expr)] [(CL3.Ident, CL3.Expr)] where
  desugar bs = do
    let (ns, es) = unzip bs
    es' <- mapM desugar es
    pure $ zip ns es'

instance Desugar Fun CL3.Fun where
  desugar _ = undefined
