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
    keywords = Set.fromList
      ["effect", "fun", "op", "match", "case", "local"
      , "free", "scoped"
      ]

inParens :: Parser a -> Parser a
inParens = between (tok "(") (tok ")")

inBraces :: Parser a -> Parser a
inBraces = between (tok "{") (tok "}")

inAngles :: Parser a -> Parser a
inAngles = between (tok "<") (tok ">")

inBrackets :: Parser a -> Parser a
inBrackets = between (tok "[") (tok "]")

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
  TyCtor <$> tyCtor <|>
  TyFun <$> tyFun <|>
  TyVar <$> identifier lower

tyCtor :: Parser TyCtor
tyCtor = do
  name <- identifier upper
  args <- option [] $ inAngles $ list (tok ",") monoTy
  ctorLt <- option LtFree (tok "'" *> lt)
  pure MkTyCtor { name, lt = ctorLt, args }

tyFun :: Parser TyFun
tyFun = do
  ctx <- option [] do
    tok "context"
    inParens effRow
  args <- inParens $ list (tok ",") monoTy
  funLt <- option LtFree (tok "'" *> lt)
  tok "->"
  res <- monoTy
  pure MkTyFun { ctx, lt = funLt, args, res }

effRow :: Parser EffRow
effRow = list (tok ",") monoTy

tyParam :: Parser TyParam
tyParam =
  MkTyParam <$>
  identifier lower <*>
  option top (tok "<:" *> monoTy)

tySchema :: Parser TySchema
tySchema = do
  ltParams <- option [] $ inBrackets $ list (tok ",") (identifier lower)
  tyParams <- option [] $ inAngles $ list (tok ",") tyParam
  ty <- monoTy
  pure MkTySchema { ltParams, tyParams, ty }

atom :: Parser Expr
atom =
  inParens expr <|>
  fun <|>
  Match <$> match <|>
  Const <$> number <|>
  Var <$> identifier lower

expr :: Parser Expr
expr =
  Ctor <$> try ctor <|>
  App <$> try app <|>
  TApp <$> try tApp <|>
  atom

fun :: Parser Expr
fun = do
  ctxParams <- option [] do
    tok "context"
    inParens $ list (tok ",") param
  tok "fun"
  ltParams <- option [] $ inBrackets $ list (tok ",") $ identifier lower
  tyParams <- option [] $ inAngles $ list (tok ",") tyParam
  params <- inParens $ list (tok ",") param
  body <- expr
  pure $ TLam MkTLam
    { ltParams, tyParams
    , body = Lam MkLam { ctxParams, params, body }
    }

tApp :: Parser TApp
tApp = do
  lhs <- atom
  ltArgs <- option [] $ inBrackets $ list (tok ",") lt
  tyArgs <- option [] $ inAngles $ list (tok ",") monoTy
  pure MkTApp { lhs, ltArgs, tyArgs }

ctor :: Parser Ctor
ctor = do
  name <- identifier upper
  ltArgs <- option [] $ inBrackets $ list (tok ",") lt
  tyArgs <- option [] $ inAngles $ list (tok ",") monoTy
  args <- inParens $ list (tok ",") expr
  pure MkCtor { name, ltArgs, tyArgs, args }

param :: Parser Param
param = do
  name <- identifier lower
  tok ":"
  ty <- monoTy
  pure MkParam { name, ty }

app :: Parser App
app = do
  callee <- atom
  (ctxArgs, args) <- inParens do
    firstArgs <- list (tok ",") expr
    optionMaybe (tok ";") >>= \case
      Nothing -> pure ([], firstArgs)
      Just _ -> do
        secondArgs <- list (tok ",") expr
        pure (firstArgs, secondArgs)
  pure MkApp { callee, ctxArgs, args }

match :: Parser Match
match = do
  tok "match"
  scrutinee <- expr
  branches <- inBraces $ some branch
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
  ltParams <- option [] $ inBrackets $ list (tok ",") $ identifier lower
  params <- option [] $ inParens $ list (tok ",") monoTy
  pure MkDataCtor { ctorName, ltParams, params }

effDecl :: Parser EffDecl
effDecl = do
  tok "effect"
  effName <- identifier upper
  tyParams <- option [] $ inAngles $ list (tok ",") $ identifier lower
  ops <- Map.fromList <$> inBraces (some opSig)
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
