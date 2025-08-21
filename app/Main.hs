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
import Data.Map ((!?))
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
    let (effCtx, tyCtx) = collectDecls prog
    let types = typeLets effCtx tyCtx prog
    case types !? target of
      Nothing -> putStrLn $ "Name " <> target <> " not found"
      Just ty -> putStrLn $ "Type: " <> show ty
  _ -> putStrLn "Unknown command"
