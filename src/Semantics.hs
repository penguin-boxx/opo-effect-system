module Semantics where

import Control.Monad.Cont
import Data.Foldable
import Data.Function (fix, on)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Optics
import Syntax
import GHC.Generics
import GHC.Stack
import Text.PrettyPrint qualified as PP
import Text.PrettyPrint.GenericPretty

type Context = Map VarName Value

newtype Continuation = WrapContinuation (Value -> Value)

instance Semigroup Continuation where
  WrapContinuation k <> WrapContinuation k' = WrapContinuation (k . k')

instance Monoid Continuation where
  mempty = WrapContinuation id

data Value = Number Int | Closure Context VarName Expr | Continuation Continuation

data InstalledHandler = InstalledHandler
  { ops :: [OpHandler]
  , kPrev :: Continuation
  }

eval :: HasCallStack => [InstalledHandler] -> Context -> Expr -> Cont Value Value
eval hStack ctx expr = case expr of
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
      Continuation (WrapContinuation f) -> pure $ f arg'
      Closure closureCtx name body ->
        let ctx' = Map.insert name arg' (ctx <> closureCtx) in
        eval hStack ctx' body
      other -> error $ concat
        [ "Expected function, got ", show other
        , " (arg=", show arg'
        , ", expr=", show expr
        , ", ctx=", show ctx, ")"
        ]
  Do targetOpName arg -> do
    arg' <- rec arg
    cont \(WrapContinuation -> kUntilNearestHandler) ->
      let LookupHandlerResult{..} = hStack `lookupHandler` targetOpName in
      let k = Continuation $ kUntilFoundHandler <> kUntilNearestHandler in
      let OpHandler{..} = foundHandler in
      let ctx' = Map.insert paramName arg' $ Map.insert kName k ctx in
      runCont (eval restHStack ctx' opBody) (view coerced kFoundHandler)
  Handle{..} ->
    cont \k ->
      let h = InstalledHandler{ kPrev = WrapContinuation k, .. } in
      let PureHandler{..} = pure in
      let scopeWithPure = (pureName =. scope) pureBody in
      runCont (eval (h : hStack) ctx scopeWithPure) id
  where
    rec = eval hStack ctx

    unwrapNumber :: HasCallStack => Value -> Int
    unwrapNumber = \case 
      Number value -> value
      other -> error $ "Expected number, got " <> show other 

data LookupHandlerResult = LookupHandlerResult
  { foundHandler :: OpHandler
  , kFoundHandler :: Continuation
  , kUntilFoundHandler :: Continuation
  , restHStack :: [InstalledHandler]
  }
  deriving stock (Show, Generic)

lookupHandler :: HasCallStack => [InstalledHandler] -> OpName -> LookupHandlerResult
lookupHandler hStack targetOpName = case hStack of
  [] -> error $ "No handler for " <> targetOpName <> " found"
  InstalledHandler{..} : restHStack 
    | Just foundHandler <- find (\OpHandler{..} -> opName == targetOpName) ops ->
      LookupHandlerResult{ kFoundHandler = kPrev, kUntilFoundHandler = mempty, .. }
  InstalledHandler{..} : restHStack ->
    over #kUntilFoundHandler (<> kPrev) (restHStack `lookupHandler` targetOpName)

eval' :: Expr -> Value
eval' expr = runCont (eval [] Map.empty expr) id

instance (Out k, Out v) => Out (Map k v) where
  doc = doc . Map.toList
  docPrec = const doc
instance Out Continuation where
  doc = const $ PP.text "<cont>"
  docPrec = const doc
instance Show Continuation where
  show = pretty
deriving stock instance Generic Value
instance Out Value
instance Show Value where
  show = pretty
deriving stock instance Generic InstalledHandler
instance Out InstalledHandler
instance Show InstalledHandler where
  show = pretty
