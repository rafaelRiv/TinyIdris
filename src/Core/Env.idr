module Core.Env

import Core.TT
import Data.List

public export
data Env : (tm : List Name -> Type) -> List name -> Type where
  Nil : Env tm []
  (::) : Binder (tm vars) -> Env tm vars -> Env tm (x :: vars)
