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
    prog <- parseProg <$> readFile fileName
    forM_ prog print
  ["type", target] -> do
    prog <- parseProg <$> readFile fileName
    result <-
      let (effCtx, tyCtx) = collectDecls prog in
      let ?effCtx = effCtx in let ?tyCtx = tyCtx in
      let expr = head [body | VarDecl MkVarDecl { name, body } <- prog, name == target] in
      runExceptT $ inferExpr expr
    case result of
      Left err -> putStrLn $ "Type error: " <> err
      Right ty -> putStrLn $ "Type: " <> show ty
  _ -> putStrLn "Unknown command"
