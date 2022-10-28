{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module L3.Desugar where

import Control.Monad.Except
import Control.Monad.State.Strict
-- import qualified Data.DList as L
-- import Data.List (isPrefixOf)
-- import qualified Data.Map.Strict as M
import Data.Text (Text)
import L3.AST

data DesugarError
  = ReservedVariablePattern (Maybe Text)
  | ExpectedExpression
  | InvalidState Text
  | InvalidCall Text
  deriving (Show, Eq)

type DesugarState = ExceptT DesugarError (State Int)

class Desugar a b where
  desugar :: a -> DesugarState b

freshVariable :: DesugarState Text
freshVariable = undefined

instance Desugar Expr Expr where
  desugar (EProg (Program es e)) = case es of
    [] -> desugar e
    (ne : es') -> case ne of
      (EDef (Def n ne')) -> do
        ne'' <- desugar ne'
        let bs = [(n, ne'')]
        es'' <- desugar (EProg (Program es' e))
        pure $ ELet (Let bs [es''])
      (EDef (DefRec n ne')) -> do
        ne'' <- desugar ne'
        es'' <- desugar (EProg (Program es' e))
        pure $ ELet (LetRec [(n, ne'')] [es''])
      _ -> desugar (EBegin (Begin [ne, (EProg (Program es' e))]))
  desugar (EBegin (Begin [])) = throwError $ InvalidCall "Begin expressions cannot be empty"
  desugar (EBegin (Begin [b])) = desugar b
  desugar (EBegin (Begin (b : bs))) = do
    b' <- desugar b
    t <- freshVariable
    bs' <- desugar (EBegin (Begin bs))
    pure $ ELet (Let [(t, b')] [bs'])
  desugar (ELet (Let bs es)) = do
    bs' <- mapM (\(n, e) -> (\e' -> (n, e')) <$> desugar e) bs
    es' <- desugar (EBegin (Begin es))
    pure $ ELet (Let bs' [es'])
  desugar (ELet (LetStar [] es)) = desugar (EBegin (Begin es))
  desugar (ELet (LetStar (b : bs) es)) = desugar (ELet (Let [b] [(ELet (LetStar bs es))]))
  desugar (ELet (LetRec bs es)) = do
    bs' <- mapM (\(n, Fun as sa) -> undefined) bs
    undefined
  desugar e = pure e

instance Desugar Fun Fun where
  desugar e = undefined
