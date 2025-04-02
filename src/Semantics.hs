module Semantics where

import Control.Monad.Cont
import Data.Function (fix, on)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Syntax
import GHC.Stack

type Context = Map VarName Value

data Value = Number Int | Closure Context VarName Expr | Continuation (Value -> Value)
  deriving Show

instance Show (a -> b) where
  show _ = "<fun>"

data Handler = Handler OpName Expr (Value -> Value)

eval :: HasCallStack => [Handler] -> Context -> Expr -> Cont Value Value
eval hStack ctx = \case
  Const value -> 
    pure $ Number value
  Plus lhs rhs -> do
    lhs' <- rec lhs
    rhs' <- rec rhs
    pure $ Number $ unwrapNumber lhs' + unwrapNumber rhs'
  Var name ->
    let msg = "No such variable " <> name in
    pure $ fromMaybe (error msg) (ctx !? name)
  Lam name body ->
    pure $ Closure ctx name body
  f :@ arg -> do
    f' <- rec f
    arg' <- rec arg
    case f' of
      Continuation f -> pure $ f arg'
      Closure closureCtx name body ->
        let ctx' = Map.insert name arg' (ctx <> closureCtx) in
        eval hStack ctx' body
      other -> error $ "Expected function, got " <> show other
  Do opName arg -> do
    arg' <- rec arg
    cont \kUntilFirstHandler ->
      let (Handler _ body kHandler, hStack', kSkipedHandlers) = hStack `lookupHandler` opName in
      let k = Continuation $ kSkipedHandlers . kUntilFirstHandler in
      let ctx' = Map.insert "p" arg' $ Map.insert "k" k ctx in
      runCont (eval hStack' ctx' body) kHandler
  Handle scope opName pName kName body ->
    cont \k ->
      let h = Handler opName body k in
      runCont (eval (h : hStack) ctx scope) id
  where
    rec = eval hStack ctx

    unwrapNumber :: HasCallStack => Value -> Int
    unwrapNumber = \case 
      Number value -> value
      other -> error $ "Expected number, got " <> show other 

    lookupHandler :: HasCallStack => [Handler] -> OpName -> (Handler, [Handler], Value -> Value)
    lookupHandler hs opName = case hs of
      [] -> error $ "No handler for " <> opName <> " found"
      h@(Handler opName' _ _) : hStack | opName == opName' -> (h, hStack, id)
      Handler _ _ k : hStack -> (. k) <$> hStack `lookupHandler` opName

eval' :: Expr -> Value
eval' expr = runCont (eval [] Map.empty expr) id
