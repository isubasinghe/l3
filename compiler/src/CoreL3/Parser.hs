{-# LANGUAGE OverloadedStrings #-}

module CoreL3.Parser where

import Control.Monad (void)
import qualified CoreL3.AST as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (dbg, dbg')

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

primparens :: Parser a -> Parser a
primparens = between (symbol "(@") (symbol ")")

atLeast :: Int -> Parser a -> Parser [a]
atLeast n p = do
  xs <- count n p
  xs' <- many p
  pure $ xs ++ xs'

pjoin :: Parser a -> Parser b -> Parser (a, b)
pjoin p1 p2 = do
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
    "not",
    "+",
    "-",
    "*",
    "xor",
    "shl",
    "shr",
    "/",
    "%",
    "<",
    "<=",
    "=",
    "id"
  ]

pprimnames :: Parser A.PrimOp
pprimnames =
  (const A.Plus) <$> C.string "+"
    <|> (const A.Xor) <$> C.string "xor"
    <|> (const A.Shl) <$> C.string "shl"
    <|> (const A.Shr) <$> C.string "shr"
    <|> (const A.Div) <$> C.string "/"
    <|> (const A.Mod) <$> C.string "%"
    <|> (const A.Less) <$> C.string "<"
    <|> (const A.Leq) <$> C.string "<="
    <|> (const A.Eq) <$> C.string "="
    <|> (const A.Id) <$> C.string "id"

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

punit :: Parser ()
punit = C.space *> do (const ()) <$> C.string "()"

pbool :: Parser Bool
pbool =
  C.space *> do
    (const True) <$> C.string "#t"
      <|> (const False) <$> C.string "#f"

pint :: Parser Int
pint =
  C.space *> do
    isNeg <- optional $ C.string "-"
    dec <- lexeme L.decimal
    case isNeg of
      Just _ -> pure $ -1 * dec
      _ -> pure $ dec

pfloat :: Parser Double
pfloat = lexeme L.float

pcharlit :: Parser Char
pcharlit = (lexeme . try) (p >>= check)
  where
    p = C.space *> do lexeme L.charLiteral
    check :: Char -> Parser Char
    check x =
      if or [x == '(', x == ')']
        then fail $ "parens " <> show x <> " cannot be used as char literal without escaping"
        else pure x

pdquotes :: Parser a -> Parser a
pdquotes = between (single '"') (single '"')

pstrlit :: Parser Text
pstrlit = do
  content <- pdquotes $ takeWhileP Nothing (/= '"')
  -- Hijack haskell's string lexer so we don't have to deal with escaping
  pure $ T.pack (read ('"' : T.unpack content ++ "\""))

pprogramitem :: Parser A.ProgramItem
pprogramitem =
  (A.PDefinition <$> (pdefrec <|> pdef))
    <|> (A.PExpr <$> pexpr)

pprogram :: Parser A.Program
pprogram = do
  is <- many $ C.space *> pprogramitem
  e <- C.space *> pexpr
  pure $ A.Program is e

pdef :: Parser A.Definition
pdef = parens $ do
  _ <- C.space *> C.string "def"
  ident <- C.space *> pidentifier
  e <- C.space *> pexpr
  pure $ A.Def ident e

pdefrec :: Parser A.Definition
pdefrec = parens $ do
  _ <- C.space *> C.string "defrec"
  ident <- C.space *> pidentifier
  fn <- C.space *> pfun
  pure $ A.DefRec ident fn

pfun :: Parser A.Fun
pfun = parens $ do
  _ <- C.space *> C.string "fun"
  is <- C.space *> (parens $ many pidentifier)
  es <- C.space *> pexprs
  pure $ A.Fun is es

plet :: Parser A.Let
plet = parens $ do
  _ <- C.space *> C.string "let"
  bs <- parens $ many (parens $ pjoin pidentifier (C.space *> pexpr))
  es <- C.space *> pexprs
  pure $ A.Let bs es

pletstar :: Parser A.Let
pletstar = parens $ do
  _ <- C.space *> C.string "let*"
  bs <- C.space *> (parens $ many (parens $ pjoin pidentifier (C.space *> pexpr)))
  es <- C.space *> pexprs
  pure $ A.LetStar bs es

pletrec :: Parser A.Let
pletrec = parens $ do
  _ <- C.space *> C.string "letrec"
  bs <- C.space *> (parens $ many (parens $ pjoin pidentifier (C.space *> pfun)))
  es <- C.space *> pexprs
  pure $ A.LetRec bs es

prec :: Parser A.Rec
prec = parens $ do
  _ <- C.space *> C.string "rec"
  ident <- C.space *> pidentifier
  mappings <- C.space *> (parens $ many (parens $ pjoin pidentifier (C.space *> pexpr)))
  exprs <- C.space *> pexprs
  pure $ A.Rec ident mappings exprs

pbegin :: Parser A.Begin
pbegin = parens $ do
  _ <- C.space *> C.string "begin"
  es <- C.space *> many (C.space *> pexpr)
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
  es <- some (C.space *> parens ((\s -> (head s, tail s)) <$> (atLeast 2 (C.space *> pexpr))))
  pure $ A.Cond es

pand :: Parser A.And
pand = parens $ do
  _ <- C.space *> C.string "and"
  es <- C.space *> (atLeast 2 (C.space *> pexpr))
  pure $ A.And es

por :: Parser A.Or
por = parens $ do
  _ <- C.space *> C.string "or"
  es <- C.space *> (atLeast 2 (C.space *> pexpr))
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
  try (A.EFun <$> pfun)
    <|> try (A.ELet <$> pletstar)
    <|> try (A.ELet <$> pletrec)
    <|> try (A.ELet <$> plet)
    <|> try (A.ERec <$> prec)
    <|> try (A.EBegin <$> pbegin)
    <|> try (A.EIf <$> pif)
    <|> try (A.ECond <$> pcond)
    <|> try (A.EAnd <$> pand)
    <|> try (A.EOr <$> por)
    <|> try (A.ENot <$> pnot)
    <|> try (A.EApp <$> papp)
    <|> try (A.EIdent <$> pidentifier)
    <|> try (A.ENum <$> pint)
    <|> try (A.EStr <$> pstrlit)
    <|> try (A.EChr <$> pcharlit)
    <|> try (A.EBool <$> pbool)
    <|> ((const A.EUnit) <$> punit)

pexprs :: Parser [A.Expr]
pexprs = do
  e <- pexpr
  es <- many $ pexpr
  pure $ [e] ++ es
