module Main where

import Common
import Driver
import Syntax
import Semantics
import Types
import Typing
import TypingCtx
import TypingUtils

import Control.Monad
import Control.Monad.Except
import Data.Maybe
import Data.Foldable
import Data.List qualified as List
import Data.Set qualified as Set
import System.Environment
import Optics

fileName :: String
fileName = "test.co"

main :: IO ()
main = getArgs >>= \case
  ["ast"] -> do
    prog :: Prog <- runSyntaxAnalisys <$> readFile fileName
    forM_ prog print
  ["type", target] -> do
    prog <- runSyntaxAnalisys <$> readFile fileName
    result <-
      let ?tyCtx =
            [ TyCtxCtor MkTyCtxCtor
                { name = ctorName
                , ltParams
                , tyParams = [MkTyParam { name, bound = top } | name <- tyParams]
                , params
                , res = MkTyCtor
                  { name = tyName
                  , lt = foldr lub LtFree $
                    let ?tyCtx = [] in foldMap ((`lifetimesOn` PositivePos) . emptyTySchema) params
                  , args = TyVar <$> tyParams
                  }
                }
            | DataDecl MkDataDecl { tyName, tyParams, dataCtors } <- prog
            , MkDataCtor { ctorName, ltParams, params } <- dataCtors
            ] in
      let ?effCtx =
            [ MkEffCtxEntry { capCtor = effName <> "K", tyParams, ops, effName }
            | EffDecl MkEffDecl { effName, tyParams, ops } <- prog
            ] in
      let expr = head [body | VarDecl MkVarDecl { name, body } <- prog, name == target] in
      runExceptT $ inferExpr expr
    case result of
      Left err -> putStrLn $ "Type error: " <> err
      Right ty -> putStrLn $ "Type: " <> show ty
  _ -> putStrLn "Unknown command"
