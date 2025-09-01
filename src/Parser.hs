module Parser where

import Common
import Syntax
import Types
import Lexer

import Control.Applicative (some)
import Data.Functor
import Data.Set qualified as Set
import Data.Map qualified as Map
import Text.Parsec
import Text.Parsec.Pos
import Optics

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
      [ "effect", "op", "handle", "perform"
      , "fun", "let", "in", "match", "case"
      , "local", "free"
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
identifier initial = notKeyword $ parseToken do
  underscores <- many (char '_')
  ini <- initial
  rest <- many (alphaNum <|> char '_')
  pure $ underscores ++ ini : rest

ctorName :: Parser CtorName
ctorName = identifier upper

varNameDecl :: Parser VarName
varNameDecl = identifier lower <|> tok "_"

varName :: Parser VarName
varName = identifier lower

tyCtorName :: Parser TyName
tyCtorName = identifier upper

tyVarName :: Parser TyName
tyVarName = identifier lower

ltName :: Parser LtName
ltName = identifier lower

opName :: Parser OpName
opName = identifier lower

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
  ltFree <$ tok "free" <|>
  LtMin . Set.fromList <$> nonEmptyList (tok "+") ltName

monoTy :: Parser MonoTy
monoTy =
  TyCtor <$> tyCtor <|>
  TyFun <$> tyFun <|>
  TyVar <$> tyVarName

tyCtor :: Parser TyCtor
tyCtor = do
  name <- tyCtorName
  args <- option [] $ inAngles $ list (tok ",") monoTy
  ctorLt <- option ltFree (tok "'" *> lt)
  pure MkTyCtor { name, lt = ctorLt, args }

tyFun :: Parser TyFun
tyFun = do
  ctx <- option [] do
    tok "context"
    inParens effRow
  args <- inParens $ list (tok ",") monoTy
  funLt <- option ltFree (tok "'" *> lt) -- todo
  tok "->"
  res <- monoTy
  pure MkTyFun { ctx, lt = funLt, args, res }

effRow :: Parser EffRow
effRow = list (tok ",") monoTy

tyParam :: Parser TyParam
tyParam =
  MkTyParam <$>
  tyVarName <*>
  option top (tok "<:" *> monoTy) -- todo default

tySchema :: Parser TySchema
tySchema = do
  ltParams <- option [] $ inBrackets $ list (tok ",") ltName
  tyParams <- option [] $ inAngles $ list (tok ",") tyParam
  ty <- monoTy
  pure MkTySchema { ltParams, tyParams, ty }

atom :: Parser Expr
atom =
  inParens expr <|>
  Match <$> match <|>
  Const <$> number <|>
  Var <$> varName <|>
  Var <$> ctorName

atomWithPostfix :: Parser Expr
atomWithPostfix = do
  target <- atom
  argBlocks <- many do
    ltArgs <- option [] $ inBrackets $ list (tok ",") lt
    tyArgs <- option [] $ inAngles $ list (tok ",") monoTy
    allArgs <- inParens do
      firstArgs <- list (tok ",") expr
      optionMaybe (tok ";") >>= \case
        Nothing -> pure ([], firstArgs)
        Just _ -> do
          secondArgs <- list (tok ",") expr
          pure (firstArgs, secondArgs)
    pure (ltArgs, tyArgs, allArgs)
  pure $ foldl mkApp target argBlocks
  where
    mkApp target (ltArgs, tyArgs, (ctxArgs, args)) =
      let callee = if null ltArgs && null tyArgs then target else
            TApp MkTApp { lhs = target, ltArgs, tyArgs } in
      App MkApp { callee, ctxArgs, args }

expr :: Parser Expr
expr =
  fun <|>
  letIn <|>
  Perform <$> perform <|>
  Handle <$> handle <|>
  atomWithPostfix

fun :: Parser Expr
fun = do
  ctxParams <- option [] do
    tok "context"
    inParens $ list (tok ",") param
  tok "fun"
  ltParams <- option [] $ inBrackets $ list (tok ",") ltName
  tyParams <- option [] $ inAngles $ list (tok ",") tyParam
  params <- inParens $ list (tok ",") param
  body <- expr
  let lam = Lam MkLam { ctxParams, params, body }
  pure $ if null ltParams && null tyParams then lam else
    TLam MkTLam { ltParams, tyParams, body = lam }

letIn :: Parser Expr
letIn = do
  tok "let"
  name <- varNameDecl
  tok ":"
  ty <- monoTy
  tok "="
  e <- expr
  tok "in"
  body <- expr
  pure $ App MkApp
    { callee = Lam MkLam { ctxParams = [], params = [MkParam { name, ty }], body }
    , ctxArgs = [], args = [e]
    }

param :: Parser Param
param = do
  name <- varNameDecl
  tok ":"
  ty <- monoTy
  pure MkParam { name, ty }

match :: Parser Match
match = do
  tok "match"
  scrutinee <- expr
  branches <- inBraces $ some branch
  pure MkMatch { scrutinee, branches }

branch :: Parser Branch
branch = do
  tok "case"
  ctorName <- ctorName
  varPatterns <- option [] $ inParens $ list (tok ",") varNameDecl
  tok "->"
  body <- expr
  pure MkBranch { ctorName, varPatterns, body }

perform :: Parser Perform
perform = do
  tok "perform"
  cap <- expr
  tok "."
  opName <- opName
  tyArgs <- option [] $ inAngles $ list (tok ",") monoTy
  args <- inParens $ list (tok ",") expr
  pure MkPerform { opName, cap, tyArgs, args }

handle :: Parser Handle
handle = do
  tok "handle"
  capName <- varNameDecl
  tok ":"
  effTy <- tyCtor
  h <- handler
  body <- expr
  pure MkHandle { capName, effTy, handler = h, body }

handler :: Parser Handler
handler = inBraces $ many handlerEntry

handlerEntry :: Parser HandlerEntry
handlerEntry = do
  tok "op"
  opName <- opName
  tyParams <- option [] $ inAngles $ list (tok ",") tyVarName
  paramNames <- inParens $ list (tok ",") varNameDecl
  body <- expr
  pure MkHandlerEntry { opName, tyParams, paramNames, body }

decl :: Parser Decl
decl =
  DataDecl <$> dataDecl <|>
  EffDecl <$> effDecl <|>
  VarDecl <$> varDecl <|>
  FunDecl <$> funDecl

dataDecl :: Parser DataDecl
dataDecl = do
  tok "data"
  tyName <- tyCtorName
  tyParams <- option [] $ inAngles $ list (tok ",") tyVarName
  tok "="
  dataCtors <- list (tok "|") dataCtor
  pure MkDataDecl { tyName, tyParams, dataCtors }

dataCtor :: Parser DataCtor
dataCtor = do
  ctorName <- ctorName
  ltParams <- option [] $ inBrackets $ list (tok ",") ltName
  params <- option [] $ inParens $ list (tok ",") monoTy
  pure MkDataCtor { ctorName = ctorName, ltParams, params }

effDecl :: Parser EffDecl
effDecl = do
  tok "effect"
  effName <- tyCtorName
  tyParams <- option [] $ inAngles $ list (tok ",") tyVarName
  ops <- Map.fromList <$> inBraces (some opSig)
  pure MkEffDecl { effName, tyParams, ops }

opSig :: Parser (OpName, OpSig)
opSig = do
  tok "op"
  name <- opName
  tyParams <- option [] $ inAngles $ list (tok ",") tyVarName
  args <- inParens $ list (tok ",") monoTy
  tok ":"
  res <- monoTy
  pure (name, MkOpSig { tyParams, args, res })

varDecl :: Parser VarDecl
varDecl = do
  tok "let"
  name <- varNameDecl
  tok "="
  body <- expr
  pure MkVarDecl { name, body, expectedTy = Nothing }

funDecl :: Parser FunDecl
funDecl = do
  ctxParams <- option [] do
    tok "context"
    inParens $ list (tok ",") param
  tok "fun"
  name <- varName
  ltParams <- option [] $ inBrackets $ list (tok ",") ltName
  tyParams <- option [] $ inAngles $ list (tok ",") tyParam
  params <- inParens $ list (tok ",") param
  tok ":"
  resTy <- monoTy
  tok "="
  body <- expr
  pure MkFunDecl { name, ltParams, tyParams, ctxParams, params, body, resTy }

prog :: Parser Prog
prog = many decl <* eof
