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

mkTodoString :: String -> String
mkTodoString x = "Unimplemented function '" <> x <> "'!"

mkVarE :: String -> Exp
mkVarE = VarE . mkName

mkConE :: String -> Exp
mkConE = ConE . mkName

todoE :: Name -> Exp
todoE name = AppE
  (mkVarE "throw")
  (AppE (mkConE "TodoException")
  (LitE $ StringL $ mkTodoString $ nameBase name))

todoB :: Name -> Body
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

bndrsFromNames :: [Name] -> [TyVarBndr Specificity]
bndrsFromNames = map (`PlainTV` SpecifiedSpec)

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
  let decs = map sigToDec $ filter isSigD sigs
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

    sigToDec :: Dec -> Dec
    sigToDec (SigD nm _) = FunD nm [Clause [] (todoB nm) []]
    sigToDec _ = error "only SigD expected!"

    mapNamesToType :: [Name] -> Type
    mapNamesToType vs = ForallT (bndrsFromNames vs) [] (AppT (ConT className) $ concatTypes (ConT dataName : map VarT vs))
