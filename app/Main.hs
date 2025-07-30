module Main where

import Common
import Driver
import Syntax
import Semantics
import Types
import Typing

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.List qualified as List
import Data.Set qualified as Set
import System.Environment
import Optics

main :: IO ()
main = getArgs >>= \case
  ["ast", fileName] -> do
    prog :: Prog <- runSyntaxAnalisys <$> readFile fileName
    forM_ prog print
  ["type", fileName, target] -> do
    prog <- runSyntaxAnalisys <$> readFile fileName
    let tyCtx =
          [ TyCtxCtor MkTyCtxCtor
            { name = ctorName
            , ltParams
            , tyParams = [MkTyParam { name, bound = top } | name <- tyParams]
            , params
            , res = MkTyCtor
              { name = tyName
              , lt = LtIntersect $ fmap LtVar ltParams -- TODO add param lifetimes
              , args = TyVar <$> tyParams
              }
            }
          | DataDecl MkDataDecl { tyName, tyParams, dataCtors } <- prog
          , MkDataCtor { ctorName, ltParams, params } <- dataCtors
          ]
    let expr = head [body | VarDecl MkVarDecl { name, body } <- prog, name == target]
    let ty = runExcept $ inferExpr [] tyCtx expr
    case ty of
      Left err -> putStrLn $ "Type error: " <> show err
      Right ty -> putStrLn $ "Type: " <> show ty
  _ -> putStrLn "Unknown command"
