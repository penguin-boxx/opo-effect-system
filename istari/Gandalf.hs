module Gandalf (castInstanceSpell) where

import Control.Monad
import Data.Map (Map)
import Data.Map qualified as M
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

undefE :: Exp
undefE = VarE $ mkName "undefined"

undefB :: Body
undefB = NormalB undefE

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

castInstanceSpell :: Name -> Name -> Q [Dec]
castInstanceSpell n cls = do
  ClassI (ClassD _ _ params _ sigs) _ <- reify cls
  TyConI (DataD _ _ bndrs _ _ _) <- reify n
  let kind = getKindOrFail params
  let kindN = kindToInt kind
  let kindM = length bndrs
  let decs = map (sigToDec knownFunctions) $ filter isSigD sigs
  instanceType <-
    if kindN == kindM + 1
      then pure $ AppT (asDataType cls) (asDataType n)
      else mapNamesToType 
                <$> (replicateM (kindM - kindN + 1) (newName "a") :: Q [Name])
  pure [InstanceD Nothing [] instanceType decs]
  where
    isSigD :: Dec -> Bool
    isSigD d = case d of
      SigD _ _ -> True
      _ -> False

    sigToDec :: Map String Dec -> Dec -> Dec
    sigToDec dict (SigD nm _) = case M.lookup (nameBase nm) dict of
      Just d -> d
      Nothing -> FunD nm [Clause [] undefB []]
    sigToDec _ _ = error "only SigD expected!"

    mapNamesToType :: [Name] -> Type
    mapNamesToType vs = ForallT (bndrsFromNames vs) [] (AppT (ConT cls) $ concatTypes (ConT n : map VarT vs))

    fromBody :: String -> Body -> (String, Dec)
    fromBody m b = (m, FunD (mkName m) [Clause [] b []])

    knownFunctions :: Map String Dec
    knownFunctions =
      M.fromList
        [ fromBody "return" $ NormalB $ VarE $ mkName "pure",
          fromBody ">>" $ NormalB $ VarE $ mkName "*>"
        ]
