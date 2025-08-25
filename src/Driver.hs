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
import Control.Exception
import Data.Function
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
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

typecheck :: EffCtx -> TyCtx -> Prog -> IO (Map String TySchema)
typecheck effCtx tyCtx prog = fold . reverse <$> flip evalStateT tyCtx do
  forM (each % _VarDecl `toListOf` prog) $ \MkVarDecl{ name, body, expectedTy } -> do
    tyCtx <- get
    let recursion = case expectedTy of
          Nothing -> []
          Just ty -> [TyCtxVar MkTyCtxVar { name, tySchema = ty }]
    let ?effCtx = effCtx
    let ?tyCtx = recursion ++ tyCtx
    tySchema <- runExceptT (inferExpr body) >>= either error pure
    when (isJust expectedTy && not (tySchema `subTySchemaOf` fromJust expectedTy)) $
      liftIO $ throwIO $ userError $
        "Unexpected type for '" <> name <> "':\n" <> show tySchema <> "\n not subtype of \n" <> show (fromJust expectedTy)
    modify (TyCtxVar MkTyCtxVar { name, tySchema } :)
    pure $ Map.singleton name tySchema
