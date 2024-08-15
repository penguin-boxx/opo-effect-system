-- | Генерация placeholder-инстансов.
module TodoMacro
  ( -- reexports support generated code
    module Control.Exception
  , module TodoException
  , todoImpl
  ) where

import Control.Exception (throw)
import Control.Monad
import Language.Haskell.TH
import TodoException (TodoException (..))

type DataName = String
type FunctionName = String
type ClassName = String

data EntryInfo = EntryInfo
  { dataName :: DataName
  , className :: ClassName
  , functionName :: FunctionName
  }

mkTodoString :: EntryInfo -> String
mkTodoString EntryInfo {..} = className <> "." <> functionName <> " for " <> dataName

mkVarE :: String -> Exp
mkVarE = VarE . mkName

todoE :: EntryInfo -> Exp
todoE info = AppE (mkVarE "todo") (LitE $ StringL $ mkTodoString info)

todoB :: EntryInfo -> Body
todoB = NormalB . todoE

nKindArgs :: Kind -> Int
nKindArgs StarT = 1
nKindArgs (AppT _ rest) = 1 + nKindArgs rest
nKindArgs _ = error "Unexpected kind form"

isSigD :: Dec -> Bool
isSigD = \case
  SigD _ _ -> True
  _ -> False

sigToDec :: DataName -> ClassName -> Dec -> Dec
sigToDec dn cn (SigD nm _) = FunD nm [Clause [] (todoB (EntryInfo dn cn (nameBase nm))) []]
sigToDec _ _ _ = error "only SigD expected!"

mkInstanceTarget :: Name -> Name -> Int -> Int -> Q Type
mkInstanceTarget className dataName nExpectedTyArgs nDataTyArgs = do
  let nSpecializeVars = nDataTyArgs - nExpectedTyArgs + 1
  vars <- replicateM nSpecializeVars (VarT <$> newName "a")
  let specializedData = foldl AppT (ConT dataName) vars
  pure $ AppT (ConT className) specializedData

todoImpl :: Name -> Name -> Q [Dec]
todoImpl className dataName = do
  (kind, sigs) <- reify className >>= \case
    ClassI (ClassD _ _ [KindedTV _ _ kind] _ sigs) _ -> pure (kind, sigs)
    x -> fail $ "expected type class, got " <> show x
  nTyArgs <- reify dataName >>= \case
    TyConI (DataD _ _ tyArgs _ _ _) -> pure $ length tyArgs
    TyConI (NewtypeD _ _ tyArgs _ _ _) -> pure $ length tyArgs
    TyConI (TySynD (nameBase -> "->") [] _) -> pure 2
    x -> fail $ "unsupported data, got " <> show x
  target <- mkInstanceTarget className dataName (nKindArgs kind) nTyArgs
  let decls = map (sigToDec (show dataName) (show className)) $ filter isSigD sigs
  pure [InstanceD Nothing [] target decls]
