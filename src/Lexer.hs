module Lexer where

import Data.Function ((&))
import Data.Char qualified as Char
import Data.List qualified as List
import Debug.Trace

-- TODO: preserve locations
type Token = String

tokenize :: String -> [Token]
tokenize str = str
  & lines
  & fmap (concatMap (splitBySelector selector) . words)
  & concatMap removeComments
  where
    selector :: [Char] -> [Char]
    selector s
      | Just prefix <- List.find (`List.isPrefixOf` s) symbols = prefix
      | c:_ <- s, isDelimiter c = [c]
      | otherwise = []
    symbols = ["->", "<:", "//"]
    delimiters = [Char.isMark, Char.isPunctuation, Char.isSymbol]
    isDelimiter c = any ($ c) delimiters
    removeComments = List.takeWhile (/= "//")

splitBySelector :: ([a] -> [a]) -> [a] -> [[a]]
splitBySelector selectFromPrefix xs = case breakSelected xs of
  (prefix, selected, suffix) -> consNotNull prefix $ consNotNull selected $
    if null suffix then [] else splitBySelector selectFromPrefix suffix
  where
    consNotNull xs
      | null xs = id
      | otherwise = (xs :)

    breakSelected xs = case (xs, selectFromPrefix xs) of
      ([], _) -> ([], [], [])
      (x:xs, []) -> case breakSelected xs of (prefix, ys, zs) -> (x:prefix, ys, zs)
      (xs, ys) -> ([], ys, drop (length ys) xs)
