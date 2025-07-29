module Parser where

import Common
import Syntax
import Types
import Lexer

import Control.Applicative (some)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Text.Parsec
import Text.Parsec.Pos

type Parser a = Parsec [Token] () a

type TokenParser a = Parsec Token () a

tokSatMap :: (Token -> Maybe a) -> Parser a
tokSatMap = tokenPrim id update
  where
    update pos t _ = updatePosString pos t

tokSat :: (Token -> Bool) -> Parser Token
tokSat p = tokSatMap testToken
  where
    testToken t = if p t then Just t else Nothing

tok :: Token -> Parser Token
tok t = tokSat (== t)

parseToken :: TokenParser a -> Parser a
parseToken p = do
  source <- sourceName <$> getPosition
  tokSatMap $ either (const Nothing) Just . parse p source

notKeyword :: Parser Token -> Parser Token
notKeyword p = p >>= \t ->
  if t `Set.member` keywords then
    unexpected $ "keyword '" <> t <> "'"
  else
    pure t
  where
    keywords = Set.fromList ["effect", "fun", "op", "match", "case", "local", "free", "scoped"]

inParens :: Parser a -> Parser a
inParens = between (tok "(") (tok ")")

inBraces :: Parser a -> Parser a
inBraces = between (tok "{") (tok "}")

inAngles :: Parser a -> Parser a
inAngles = between (tok "<") (tok ">")

identifier :: TokenParser Char -> Parser VarName
identifier initial = notKeyword $ parseToken $
  (:) <$> initial <*> many (alphaNum <|> char '_')

number :: Parser Int
number = read <$> parseToken (some digit)

list :: Parser delimiter -> Parser element -> Parser [element]
list delimiter element = optionMaybe element >>= \case
  Nothing -> pure []
  Just parsed -> (parsed :) <$> many (delimiter *> element)

nonEmptyList :: Parser delimiter -> Parser element -> Parser [element]
nonEmptyList delimiter element = (:) <$> element <*> many (delimiter *> element)


lt :: Parser Lt
lt =
  LtLocal <$ tok "local" <|>
  LtFree <$ tok "free" <|>
  LtVar <$> identifier lower <|>
  LtIntersect <$> (tok "&" *> inParens (list (tok ",") lt)) <|>
  inParens lt

monoTy :: Parser MonoTy
monoTy =
  TyCtor <$> try tyCtor <|>
  TyFun <$> tyFun <|>
  TyVar <$> identifier lower

tyCtor :: Parser TyCtor
tyCtor = do
  ctorLt <- option LtFree do
    tok "scoped"
    inParens lt
  name <- identifier upper
  args <- option [] $ inAngles $ list (tok ",") monoTy
  pure MkTyCtor { name, lt = ctorLt, args }

tyFun :: Parser TyFun
tyFun = do
  funLt <- option LtFree do
    tok "scoped"
    inParens lt
  ctx <- option [] do
    tok "context"
    inParens effRow
  args <- inParens $ list (tok ",") monoTy
  tok "->"
  res <- monoTy
  pure MkTyFun { ctx, lt = funLt, args, res }

effRow :: Parser EffRow
effRow = list (tok ",") monoTy

tyParam :: Parser TyParam
tyParam =
  MkTyParam <$>
  identifier lower <*>
  option
    (TyCtor MkTyCtor { name = "Any", lt = LtLocal, args = [] })
    (tok "<:" *> monoTy)

tySchema :: Parser TySchema
tySchema = do
  tok "<"
  ltParams <- list (tok ",") $ identifier lower
  tok ";"
  tyParams <- list (tok ",") tyParam
  tok ">"
  ty <- monoTy
  pure MkTySchema { ltParams, tyParams, ty }

atom :: Parser Expr
atom =
  inParens expr <|>
  TLam <$> tLam <|>
  Lam <$> lam <|>
  Match <$> match <|>
  Const <$> number <|>
  Var <$> identifier lower

expr :: Parser Expr
expr =
  TApp <$> try tApp <|>
  Ctor <$> try ctor <|>
  App <$> try app <|>
  atom

tLam :: Parser TLam
tLam = do
  tok "\\\\"
  tok "<"
  ltParams <- list (tok ",") $ identifier lower
  tok ";"
  tyParams <- list (tok ",") tyParam
  tok ">"
  tok "->"
  body <- expr
  pure MkTLam { ltParams, tyParams, body }

tApp :: Parser TApp
tApp = do
  lhs <- atom
  tok "<"
  ltArgs <- list (tok ",") lt
  tok ";"
  tyArgs <- list (tok ",") monoTy
  tok ">"
  pure MkTApp { lhs, ltArgs, tyArgs }

ctor :: Parser Ctor
ctor = do
  name <- identifier upper
  tok "<"
  ltArgs <- list (tok ",") lt
  tok ";"
  tyArgs <- list (tok ",") monoTy
  tok ">"
  args <- inParens $ list (tok ",") expr
  pure MkCtor { name, ltArgs, tyArgs, args }

lam :: Parser Lam
lam = do
  ctxParams <- option [] do
    tok "context"
    inParens $ list (tok ",") param
  tok "\\"
  params <- inParens $ list (tok ",") param
  tok "->"
  body <- expr
  pure MkLam { ctxParams, params, body }

param :: Parser Param
param = do
  name <- identifier lower
  tok ":"
  ty <- monoTy
  pure MkParam { name, ty }

app :: Parser App
app = do
  callee <- atom
  tok "("
  ctxArgs <- list (tok ",") expr
  tok ";"
  args <- list (tok ",") expr
  pure MkApp { callee, ctxArgs, args }

match :: Parser Match
match = do
  tok "match"
  scrutinee <- expr
  tok "{"
  branches <- some branch
  tok "}"
  pure MkMatch { scrutinee, branches }

branch :: Parser Branch
branch = do
  tok "case"
  ctorName <- identifier upper
  varPatterns <- inParens $ list (tok ",") $ identifier lower
  tok "->"
  body <- expr
  pure MkBranch { ctorName, varPatterns, body }

decl :: Parser Decl
decl =
  DataDecl <$> dataDecl <|>
  EffDecl <$> effDecl <|>
  VarDecl <$> varDecl

dataDecl :: Parser DataDecl
dataDecl = do
  tok "data"
  tyName <- identifier upper
  tyParams <- option [] $ inAngles $ list (tok ",") $ identifier lower
  tok "="
  dataCtors <- list (tok "|") dataCtor
  pure MkDataDecl { tyName, tyParams, dataCtors }

dataCtor :: Parser DataCtor
dataCtor = do
  ctorName <- identifier upper
  ltParams <- option [] $ inAngles $ list (tok ",") $ identifier lower
  params <- option [] $ inParens $ list (tok ",") monoTy
  pure MkDataCtor { ctorName, ltParams, params }

effDecl :: Parser EffDecl
effDecl = do
  tok "effect"
  effName <- identifier upper
  tyParams <- option [] $ inAngles $ list (tok ",") $ identifier lower
  ops <- Map.fromList <$> inBraces (many opSig)
  pure MkEffDecl { effName, tyParams, ops }

opSig :: Parser (OpName, OpSig)
opSig = do
  tok "op"
  name <- identifier lower
  tyParams <- option [] $ inAngles $ list (tok ",") tyParam
  args <- inParens $ list (tok ",") monoTy
  tok ":"
  res <- monoTy
  pure (name, MkOpSig { tyParams, args, res })

varDecl :: Parser VarDecl
varDecl = do
  tok "let"
  name <- identifier lower
  tok "="
  body <- expr
  pure MkVarDecl { name, body }

prog :: Parser Prog
prog = many decl
