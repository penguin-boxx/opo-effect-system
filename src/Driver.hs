module Driver where

import Syntax
import Lexer
import Types
import TypingCtx
import TypingUtils
import Typing
import Parser qualified

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.Function
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Parsec
import Optics

parseProg :: String -> Prog
parseProg = report . parse Parser.prog "" . tokenize
  where
    report :: Either ParseError a -> a
    report = \case
      Left err -> error $ show err
      Right res -> res

collectDecls :: Prog -> (EffCtx, TyCtx)
collectDecls prog =
  let effCtx =
        [ MkEffCtxEntry { capCtor = effName <> "K", tyParams, ops, effName }
        | EffDecl MkEffDecl { effName, tyParams, ops } <- prog
        ] in
  let tyCtx =
        [ TyCtxCtor MkTyCtxCtor
          { name = ctorName
          , ltParams
          , tyParams = [MkTyParam { name, bound = top } | name <- tyParams]
          , params
          , res = MkTyCtor
            { name = tyName
            , lt = ctorLifetime params
            , args = TyVar <$> tyParams
            }
          }
        | DataDecl MkDataDecl { tyName, tyParams, dataCtors } <- prog
        , MkDataCtor { ctorName, ltParams, params } <- dataCtors
        ] in
  (effCtx, tyCtx)
  where
    ctorLifetime params = let ?tyCtx = [] in params
      & foldMap ((`lifetimesOn` PositivePos) . emptyTySchema)
      & foldr lub LtFree

typeLets :: EffCtx -> TyCtx -> Prog -> Map String TySchema
typeLets effCtx tyCtx prog = fold $ reverse $ flip evalState tyCtx do
  let ?effCtx = effCtx
  forM (each % _VarDecl `toListOf` prog) $ \MkVarDecl{ name, body } -> do
    tyCtx <- get
    let ?tyCtx = tyCtx
    tySchema <- runExceptT (inferExpr body) >>= either error pure
    modify (TyCtxVar MkTyCtxVar { name, tySchema } :)
    pure $ Map.singleton name tySchema
