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
import Data.Foldable
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
    result <- runExceptT do
      tyCtx <- sequenceA
        [ do
            -- Bounds of generics should not affect resulting lifetime.
            let bound = TyCtor MkTyCtor { name = "Any", lt = LtFree, args = [] }
            let ltTyCtx = [TyCtxTy MkTyParam { name, bound } | name <- tyParams]
            paramLts <- fold <$> mapM (lifetimes ltTyCtx PositivePos . emptyTySchema) params
            pure $ TyCtxCtor MkTyCtxCtor
              { name = ctorName
              , ltParams
              , tyParams = [MkTyParam { name, bound = top } | name <- tyParams]
              , params
              , res = MkTyCtor
                { name = tyName
                , lt = LtIntersect $ Set.toList paramLts
                , args = TyVar <$> tyParams
                }
              }
        | DataDecl MkDataDecl { tyName, tyParams, dataCtors } <- prog
        , MkDataCtor { ctorName, ltParams, params } <- dataCtors
        ]
      let effCtx =
            [ MkEffCtxEntry { capCtor = effName <> "K", tyParams, ops, effName }
            | EffDecl MkEffDecl { effName, tyParams, ops } <- prog
            ]
      let expr = head [body | VarDecl MkVarDecl { name, body } <- prog, name == target]
      inferExpr effCtx tyCtx expr
    case result of
      Left err -> putStrLn $ "Type error: " <> err
      Right ty -> putStrLn $ "Type: " <> show ty
  _ -> putStrLn "Unknown command"
