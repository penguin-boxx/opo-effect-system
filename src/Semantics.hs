module Semantics where

import Common
import Syntax
import Control.Monad.Cont
import Data.Foldable
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.List qualified as List
import Optics
import GHC.Generics
import GHC.Stack
import Text.PrettyPrint qualified as PP
import Text.PrettyPrint.GenericPretty
import Debug.Trace

type Context = Map VarName Value

data Value
  = Number Int
  | Obj VarName [Value]
  | Closure { argName :: VarName, closBody :: Expr, closCtx :: Context }
  | Continuation { kBody :: K, kCtx :: Context }
  deriving Eq

data Frame
  = LApp Expr
  | RApp Value
  | KMatch [Branch]
  | KHandle
    { return :: Expr
    , ops :: [OpHandler]
    , ctx :: Context
    }
  | EndScope Context
  deriving stock Eq

type K = [Frame]

-- evalE :: HasCallStack => Context -> Expr -> K -> Value
-- evalE !ctx !expr !k =
--   -- trace (
--   --   replicate 5 '-' <> " " <> "evalE" <> " " <> replicate 60 '-' <>
--   --   "\nEXPR = " <> show expr <>
--   --   "\nK = " <> show k <>
--   --   "\nCTX = " <> show ctx
--   -- ) $
--   case expr of
--     Const value -> evalK ctx k (Number value)
--     Plus lhs rhs -> evalE ctx lhs (LPlus rhs : k)
--     Var name ->
--       let msg = "No such variable " <> name in
--       let value = fromMaybe (error msg) (ctx !? name) in
--       evalK ctx k value
--     Lam argName closBody -> evalK ctx k Closure{ closCtx = ctx, argName, closBody }
--     f @ arg -> evalE ctx f (LApp arg : k)
--     Pair l r -> evalE ctx l (LPair r : k)
--     Fst expr -> evalE ctx expr (KFst : k)
--     Snd expr -> evalE ctx expr (KSnd : k)
--     Do targetOpName arg -> evalE ctx arg (KDo targetOpName : k)
--     Handle{ hPure, hOps, hScope } -> evalE ctx hScope (KHandle { hCtx = ctx, hPure, hOps } : k)
--     Ascription _ _ expr -> evalE ctx expr k
--     LetIn name expr body -> evalE ctx (Lam name body @ expr) k

-- evalK :: HasCallStack => Context -> K -> Value -> Value
-- evalK !ctx !k !value =
--   -- trace (
--   --   replicate 5 '-' <> " " <> "evalK" <> " " <> replicate 60 '-' <>
--   --   "\nVALUE = " <> show value <>
--   --   "\nK = " <> show k <>
--   --   "\nCTX = " <> show ctx
--   -- ) $
--   case k of
--     [] -> value
--     LPlus rhs : k -> evalE ctx rhs (RPlus value : k)
--     RPlus lhs' : k -> evalK ctx k (Number $ unwrapNumber lhs' + unwrapNumber value)
--     LApp arg : k -> evalE ctx arg (RApp value : k)
--     RApp f' : k -> case f' of
--       Closure{ argName, closCtx, closBody } ->
--         let ctx' = Map.insert argName value closCtx in
--         evalE ctx' closBody (EndScope ctx : k)
--       Continuation{ kBody, kCtx } -> evalK kCtx (kBody ++ EndScope ctx : k) value
--       other -> error $ "Expected function, got " <> show other
--     LPair r : k -> evalE ctx r (RPair value : k)
--     RPair l' : k -> evalK ctx k (PairValue l' value)
--     KFst : k -> case value of
--       PairValue l _ -> evalK ctx k l
--       other -> error $ "Expected pair, got " <> show other
--     KSnd : k -> case value of
--       PairValue _ r -> evalK ctx k r
--       other -> error $ "Expected pair, got " <> show other
--     KDo targetOpName : k ->
--       let (OpHandler{ paramName, kName, opBody }, hCtx, kHandler, kTop) = splitK targetOpName k in
--       let opK = Continuation { kBody = kTop, kCtx = ctx } in
--       let ctx' = Map.insert paramName value $ Map.insert kName opK hCtx in
--       evalE ctx' opBody kHandler
--     KHandle{ hPure = PureHandler{ pureName, pureBody } } : k ->
--       let ctx' = Map.insert pureName value ctx in
--       evalE ctx' pureBody (EndScope ctx : k)
--     EndScope ctx' : k -> evalK ctx' k value
--   where
--     unwrapNumber :: HasCallStack => Value -> Int
--     unwrapNumber = \case
--       Number value -> value
--       other -> error $ "Expected number, got " <> show other

--     splitK :: HasCallStack => OpName -> K -> (OpHandler, Context, K, K)
--     splitK targetOpName = \case
--       [] -> error $ "No handler for " <> targetOpName <> " found"
--       frame@KHandle{ hOps, hCtx } : k
--         | Just foundHandler <- find (\OpHandler{ opName } -> opName == targetOpName) hOps ->
--           (foundHandler, hCtx, k, [frame])
--       frame : k -> (frame :) <$> splitK targetOpName k

-- eval :: HasCallStack => Expr -> Value
-- eval expr = evalE Map.empty expr []

deriving stock instance Generic Value
instance Out Value
instance Show Value where
  show = pretty
deriving stock instance Generic Frame
instance Out Frame
instance Show Frame where
  show = pretty
