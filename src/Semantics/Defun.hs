module Semantics.Defun where

import Control.Monad.Cont
import Data.Foldable
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.List qualified as List
import Optics
import Syntax
import GHC.Generics
import GHC.Stack
import Text.PrettyPrint qualified as PP
import Text.PrettyPrint.GenericPretty
import Debug.Trace

type Context = Map VarName Value

data K
  = Halt
  | LPlus Expr K
  | RPlus Value K
  | LApp Expr K
  | RApp Value K
  | KDo OpName K

instance Semigroup K where
  -- actions from r will be executed first
  l <> r = case r of
    Halt -> l
    LPlus rhs k -> LPlus rhs (l <> k)
    RPlus lhs' k -> RPlus lhs' (l <> k)
    LApp arg k -> LApp arg (l <> k)
    RApp f' k -> RApp f' (l <> k)
    KDo name k -> KDo name (l <> k)

instance Monoid K where
  mempty = Halt

data Value
  = Number Int
  | Closure { name :: VarName, body :: Expr, closCtx :: Context }
  | Continuation { kBody :: K, kCtx :: Context, kHStack :: [InstalledHandler] }

data InstalledHandler = InstalledHandler
  { handlerCtx :: Context
  , ops :: [OpHandler]
  , kPrev :: K
  }

eval :: HasCallStack => [InstalledHandler] -> Context -> Expr -> K -> Value
eval !hStack !ctx !expr !k =
  -- trace (
  --   replicate 5 '-' <> " " <> "eval" <> " " <> replicate 60 '-' <>
  --   "\nEXPR = " <> show expr <>
  --   "\nK = " <> show k <>
  --   "\nCTX = " <> show ctx <>
  --   "\nHSTACK = " <> show hStack
  -- ) $
  -- todo clear context somewhere
  case expr of
    Const value -> appK hStack ctx k (Number value)
    Plus lhs rhs -> eval hStack ctx lhs (LPlus rhs k)
    Var name ->
      let msg = "No such variable " <> name in
      let value = fromMaybe (error msg) (ctx !? name) in
      appK hStack ctx k value
    Lam name body -> appK hStack ctx k (Closure{ name, body, closCtx = ctx })
    f :@ arg -> eval hStack ctx f (LApp arg k)
    Do targetOpName arg -> eval hStack ctx arg (KDo targetOpName k)
    Handle {..} ->
      let h = InstalledHandler{ handlerCtx = ctx, kPrev = k, .. } in
      let PureHandler{..} = pure in
      let pureClos = eval hStack ctx (Lam pureName pureBody) Halt in
      let pureK = RApp pureClos Halt in
      appK hStack ctx k $ eval (h : hStack) ctx scope pureK

appK :: HasCallStack => [InstalledHandler] -> Context -> K -> Value -> Value
appK !hStack !ctx !k !value =
  -- trace (
  --   replicate 5 '-' <> " " <> "app" <> " " <> replicate 60 '-' <>
  --   "\nVALUE = " <> show value <>
  --   "\nK = " <> show k <>
  --   "\nCTX = " <> show ctx <>
  --   "\nHSTACK = " <> show hStack
  -- ) $
  case k of
    Halt -> value
    LPlus rhs k -> eval hStack ctx rhs (RPlus value k)
    RPlus lhs' k -> appK hStack ctx k (Number $ unwrapNumber lhs' + unwrapNumber value)
    LApp arg k -> eval hStack ctx arg (RApp value k)
    RApp f' k -> case f' of
      Closure{..} -> eval hStack (Map.insert name value closCtx) body k
      Continuation{..} -> appK hStack ctx k $! appK kHStack kCtx kBody value
      other -> error $ "Expected function, got " <> show other
    KDo targetOpName k ->
      let LookupHandlerResult{..} = hStack `lookupHandler` targetOpName in
      let opK = Continuation { kBody = kSkippedHandlers <> k, kCtx = ctx, kHStack = hStack } in
      let OpHandler{..} = foundHandler in
      let ctx' = Map.insert paramName value $ Map.insert kName opK handlerCtx in
      eval restHStack ctx' opBody Halt
  where
    unwrapNumber :: HasCallStack => Value -> Int
    unwrapNumber = \case
      Number value -> value
      other -> error $ "Expected number, got " <> show other

data LookupHandlerResult = LookupHandlerResult
  { foundHandler :: OpHandler
  , handlerCtx :: Context
  , kFoundHandler :: K
  , kSkippedHandlers :: K
  , restHStack :: [InstalledHandler]
  }
  deriving stock (Show, Generic)

lookupHandler :: HasCallStack => [InstalledHandler] -> OpName -> LookupHandlerResult
lookupHandler hStack targetOpName = case hStack of
  [] -> error $ "No handler for " <> targetOpName <> " found"
  InstalledHandler{..} : restHStack
    | Just foundHandler <- find (\OpHandler{..} -> opName == targetOpName) ops ->
      LookupHandlerResult{ kFoundHandler = kPrev, kSkippedHandlers = mempty, .. }
  InstalledHandler{..} : restHStack ->
    over #kSkippedHandlers (<> kPrev) (restHStack `lookupHandler` targetOpName)

eval' :: Expr -> Value
eval' expr = eval [] Map.empty expr Halt

instance (Out k, Out v) => Out (Map k v) where
  doc = doc . Map.toList
  docPrec = const doc
deriving stock instance Generic Value
instance Out Value
instance Show Value where
  show = pretty
deriving stock instance Generic K
instance Out K
instance Show K where
  show = pretty
-- deriving stock instance Generic InstalledHandler
-- instance Out InstalledHandler
-- instance Show InstalledHandler where
--   show = pretty
instance Out InstalledHandler where
  doc = const $ PP.text "<ih>"
  docPrec = const doc
instance Show InstalledHandler where
  show = pretty
