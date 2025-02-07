module Core.Env

import Core.TT
import Data.List

public export
data Env : (tm : List Name -> Type) -> List Name -> Type where
  Nil : Env tm []
  (::) : Binder (tm vars) -> Env tm vars -> Env tm (x :: vars)

public export
data IsDefined : Name -> List Name -> Type where
  MkIsDefined : {idx : Nat} -> (0 p : IsVar n idx vars) ->
                IsDefined n vars

export
defined : {vars : _} ->
          (n : Name) -> Env Term vars ->
          Maybe (IsDefined n vars)
defined n [] = Nothing
defined {vars = x :: xs} n (b :: env)
    = case nameEq n x of
           Nothing => do MkIsDefined prf <- defined n env
                         pure (MkIsDefined (Later prf))
           Just Refl => Just (MkIsDefined First)

 
