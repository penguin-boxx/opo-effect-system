module Semantics.Simple where

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

data Frame
  = LPlus Expr
  | RPlus Value
  | LApp Expr
  | RApp Value
  | LPair Expr
  | RPair Value
  | KFst
  | KSnd
  | KDo OpName
  | KHandle
    { hPure :: PureHandler
    , hOps :: [OpHandler]
    , hCtx :: Context
    }
  | EndScope Context

type K = [Frame]

data Value
  = Number Int
  | Closure { argName :: VarName, closBody :: Expr, closCtx :: Context }
  | Continuation { kBody :: K, kCtx :: Context }
  | PairValue Value Value

evalE :: HasCallStack => Context -> Expr -> K -> Value
evalE !ctx !expr !k =
  -- trace (
  --   replicate 5 '-' <> " " <> "evalE" <> " " <> replicate 60 '-' <>
  --   "\nEXPR = " <> show expr <>
  --   "\nK = " <> show k <>
  --   "\nCTX = " <> show ctx
  -- ) $
  case expr of
    Const value -> evalK ctx k (Number value)
    Plus lhs rhs -> evalE ctx lhs (LPlus rhs : k)
    Var name ->
      let msg = "No such variable " <> name in
      let value = fromMaybe (error msg) (ctx !? name) in
      evalK ctx k value
    Lam argName closBody -> evalK ctx k Closure{ closCtx = ctx, .. }
    f :@ arg -> evalE ctx f (LApp arg : k)
    Pair l r -> evalE ctx l (LPair r : k)
    Fst expr -> evalE ctx expr (KFst : k)
    Snd expr -> evalE ctx expr (KSnd : k)
    Do targetOpName arg -> evalE ctx arg (KDo targetOpName : k)
    Handle{..} -> evalE ctx hScope (KHandle { hCtx = ctx, .. } : k)

evalK :: HasCallStack => Context -> K -> Value -> Value
evalK !ctx !k !value =
  -- trace (
  --   replicate 5 '-' <> " " <> "evalK" <> " " <> replicate 60 '-' <>
  --   "\nVALUE = " <> show value <>
  --   "\nK = " <> show k <>
  --   "\nCTX = " <> show ctx
  -- ) $
  case k of
    [] -> value
    LPlus rhs : k -> evalE ctx rhs (RPlus value : k)
    RPlus lhs' : k -> evalK ctx k (Number $ unwrapNumber lhs' + unwrapNumber value)
    LApp arg : k -> evalE ctx arg (RApp value : k)
    RApp f' : k -> case f' of
      Closure{..} -> evalE (Map.insert argName value closCtx) closBody (EndScope ctx : k)
      Continuation{..} -> evalK kCtx (kBody ++ EndScope ctx : k) value
      other -> error $ "Expected function, got " <> show other
    LPair r : k -> evalE ctx r (RPair value : k)
    RPair l' : k -> evalK ctx k (PairValue l' value)
    KFst : k -> case value of
      PairValue l _ -> evalK ctx k l
      other -> error $ "Expected pair, got " <> show other
    KSnd : k -> case value of
      PairValue _ r -> evalK ctx k r
      other -> error $ "Expected pair, got " <> show other
    KDo targetOpName : k ->
      let (OpHandler{..}, hCtx, kHandler, kTop) = splitK targetOpName k in
      let opK = Continuation { kBody = kTop, kCtx = ctx } in
      let ctx' = Map.insert paramName value $ Map.insert kName opK hCtx in
      evalE ctx' opBody kHandler
    KHandle{ hPure = PureHandler{..} } : k ->
      let ctx' = Map.insert pureName value ctx in
      evalE ctx' pureBody (EndScope ctx : k)
    EndScope ctx' : k -> evalK ctx' k value
  where
    unwrapNumber :: HasCallStack => Value -> Int
    unwrapNumber = \case
      Number value -> value
      other -> error $ "Expected number, got " <> show other

    splitK :: HasCallStack => OpName -> K -> (OpHandler, Context, K, K)
    splitK targetOpName = \case
      [] -> error $ "No handler for " <> targetOpName <> " found"
      frame@KHandle{..} : k
        | Just foundHandler <- find (\OpHandler{..} -> opName == targetOpName) hOps ->
          (foundHandler, hCtx, k, [frame])
      frame : k -> (frame :) <$> splitK targetOpName k

eval :: HasCallStack => Expr -> Value
eval expr = evalE Map.empty expr []

instance (Out k, Out v) => Out (Map k v) where
  doc = doc . Map.toList
  docPrec = const doc
deriving stock instance Generic Value
instance Out Value
instance Show Value where
  show = pretty
deriving stock instance Generic Frame
instance Out Frame
instance Show Frame where
  show = pretty
