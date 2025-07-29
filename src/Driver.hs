module Driver where

import Syntax
import Lexer
import Types
import Parser qualified

import Text.Parsec

class RunSyntaxAnalysis res where
  runSyntaxAnalisys :: String -> res

instance RunSyntaxAnalysis Expr where
  runSyntaxAnalisys = report . parse Parser.expr "" . tokenize

instance RunSyntaxAnalysis MonoTy where
  runSyntaxAnalisys = report . parse Parser.monoTy "" . tokenize

report :: Either ParseError a -> a
report = \case
  Left err -> error $ show err
  Right res -> res


