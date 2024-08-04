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
mkTodoString info = className info 
  <> "." <> functionName info <> " for " <> dataName info 

mkVarE :: String -> Exp
mkVarE = VarE . mkName

todoE :: EntryInfo -> Exp
todoE info = AppE (mkVarE "todo") (LitE $ StringL $ mkTodoString info)

todoB :: EntryInfo -> Body
todoB = NormalB . todoE

kindToInt :: Kind -> Int
kindToInt ArrowT = 0
kindToInt StarT = 1
kindToInt (AppT x y) = kindToInt x + kindToInt y
kindToInt _ = error "Unexpected type"

asDataType :: Name -> Type
asDataType = ConT

concatTypes :: [Type] -> Type
concatTypes = foldl1 AppT

getKindOrFail :: [TyVarBndr ()] -> Kind
getKindOrFail [KindedTV _ _ kind] = kind
getKindOrFail _ = error "Unexpected kind"

todoImpl :: Name -> Name -> Q [Dec]
todoImpl dataName className = do
  ClassI (ClassD _ _ params _ sigs) _ <- reify className
  TyConI (DataD _ _ bndrs _ _ _) <- reify dataName
  let kind = getKindOrFail params
  let kindN = kindToInt kind
  let kindM = length bndrs
  let decs = map (sigToDec (show dataName) (show className)) $ filter isSigD sigs
  instanceType <-
    if kindN == kindM + 1
      then pure $ AppT (asDataType className) (asDataType dataName)
      else mapNamesToType 
                <$> (replicateM (kindM - kindN + 1) (newName "a") :: Q [Name])
  pure [InstanceD Nothing [] instanceType decs]
  where
    isSigD :: Dec -> Bool
    isSigD d = case d of
      SigD _ _ -> True
      _ -> False

    sigToDec :: DataName -> ClassName -> Dec -> Dec
    sigToDec dn cn (SigD nm _) = FunD nm [Clause [] (todoB (EntryInfo dn cn (nameBase nm))) []]
    sigToDec _ _ _ = error "only SigD expected!"

    mapNamesToType :: [Name] -> Type
    mapNamesToType = AppT (ConT className) . concatTypes . (ConT dataName : ) . map VarT
