module Clash.Backend.Aiger.CoreEdits (removeCase) where

import Clash.Core.Term (Term (..))
import Clash.Core.VarEnv (mapVarEnv)
import Clash.Driver.Types (Binding, BindingMap)

removeCase :: BindingMap -> BindingMap
removeCase bm = mapVarEnv replaceCase bm
  where
    replaceCase :: Binding Term -> Binding Term
    replaceCase bt = fmap rc bt
    rc (Lam variable term) = Lam variable (rc term)
    rc (TyLam tyVar term) = TyLam tyVar (rc term)
    rc (App term1 term) = App (rc term1) (rc term)
    rc (TyApp term ty) = TyApp (rc term) ty
    rc (Let (boundterm) term1) = Let (fmap rc boundterm) (rc term1)
    rc (Case term ty alts) = rcase term ty alts
    rc (Cast term ty type2) = Cast (rc term) ty type2
    rc (Tick tickInfo term) = Tick tickInfo (rc term)
    rc x = x
    rcase term ty alts = Case term ty alts
