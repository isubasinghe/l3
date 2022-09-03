{-# LANGUAGE OverloadedStrings #-}

module Frontend.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Frontend.AST as A
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space C.space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment ";"
    blockCmnt = L.skipBlockComment "#|" "|#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

atLeast :: Int -> Parser a -> Parser [a]
atLeast n p = do
  xs <- count n p
  xs' <- many p
  pure $ xs ++ xs'

pAsTuples :: Parser a -> Parser [a] -> Parser (a, [a])
pAsTuples p1 p2 = do
  out1 <- p1
  out2 <- p2
  pure $ (out1, out2)

rws :: [Text]
rws =
  [ "defrec",
    "define",
    "def",
    "fun",
    "let*",
    "letrec",
    "let",
    "rec",
    "begin",
    "if",
    "cond",
    "and",
    "or",
    "not"
  ]

pidentifier :: Parser Text
pidentifier = (lexeme . try) (p >>= check)
  where
    p =
      fmap T.pack $
        (:) <$> C.letterChar
          <*> many (C.alphaNumChar <|> single '_')
    check x =
      if x `elem` rws
        then fail $ "keyword " <> show x <> " cannot be an identifier"
        else pure x

pint :: Parser Int
pint = lexeme L.decimal

pfloat :: Parser Double
pfloat = lexeme L.float

pcharlit :: Parser Char
pcharlit = lexeme L.charLiteral

pdquotes :: Parser a -> Parser a
pdquotes = between (single '"') (single '"')

pstrlit :: Parser Text
pstrlit = do
  content <- pdquotes $ takeWhileP Nothing (/= '"')
  -- Hijack haskell's string lexer so we don't have to deal with escaping
  pure $ T.pack (read ('"' : T.unpack content ++ "\""))

pdef :: Parser A.Definition
pdef = parens $ do
  _ <- C.space *> C.string "def"
  undefined

pdefrec :: Parser A.Definition
pdefrec = parens $ do
  _ <- C.space *> C.string "defrec"
  undefined

pfun :: Parser A.Fun
pfun = parens $ do
  _ <- C.space *> C.string "fun"
  undefined

plet :: Parser A.Let
plet = parens $ do
  _ <- C.space *> C.string "let"
  undefined

pletstar :: Parser A.Let
pletstar = parens $ do
  _ <- C.space *> C.string "let*"
  undefined

pletrec :: Parser A.Let
pletrec = parens $ do
  _ <- C.space *> C.string "letrec"
  undefined

prec :: Parser A.Rec
prec = parens $ do
  _ <- C.space *> C.string "rec"
  undefined

pbegin :: Parser A.Begin
pbegin = parens $ do
  _ <- C.space *> C.string "begin"
  es <- many (C.space *> pexpr)
  pure $ A.Begin es

pif :: Parser A.If
pif = parens $ do
  _ <- C.space *> C.string "if"
  e1 <- C.space *> pexpr
  e2 <- C.space *> pexpr
  maybeE <- optional (C.space *> pexpr)
  pure $ A.If e1 e2 maybeE

pcond :: Parser A.Cond
pcond = parens $ do
  _ <- C.space *> C.string "cond"
  es <- many $ parens (pAsTuples (C.space *> pexpr) (many $ C.space *> pexpr))
  pure $ A.Cond es

pand :: Parser A.And
pand = parens $ do
  _ <- C.space *> C.string "and"
  es <- atLeast 2 (C.space *> pexpr)
  pure $ A.And es

por :: Parser A.Or
por = parens $ do
  _ <- C.space *> C.string "or"
  es <- atLeast 2 (C.space *> pexpr)
  pure $ A.Or es

pnot :: Parser A.Not
pnot = parens $ do
  _ <- C.space *> C.string "not"
  e <- C.space *> pexpr
  pure $ A.Not e

papp :: Parser A.App
papp = parens $ do
  es <- sepBy1 pexpr C.space
  pure $ A.App es

pexpr :: Parser A.Expr
pexpr =
  (A.EFun <$> pfun)
    <|> (A.ELet <$> pletstar)
    <|> (A.ELet <$> pletrec)
    <|> (A.ELet <$> plet)
    <|> (A.ERec <$> prec)
    <|> (A.EBegin <$> pbegin)
