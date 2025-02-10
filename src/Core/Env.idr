module Core.Env

import Core.TT
import Data.List

public export
data Env : (tm : List Name -> Type) -> List Name -> Type where
  Nil : Env tm []
  (::) : Binder (tm vars) -> Env tm vars -> Env tm (x :: vars)

revOnto : (xs, vs : List a) -> reverseOnto xs vs = reverse vs ++ xs
revOnto xs [] = Refl
revOnto xs (v :: vs)
    = rewrite revOnto (v :: xs) vs in
        rewrite appendAssociative (reverse vs) [v] xs in
				  rewrite revOnto [v] vs in Refl

-- Weaken by all the names at once at the end, to save multiple traversals
-- in big environments
-- Also reversing the names at the end saves significant time over concatenating
-- when environments get fairly big.
getBinderUnder : Weaken tm =>
                 {vars : _} -> {idx : Nat} ->
                 (ns : List Name) ->
                 (0 p : IsVar x idx vars) -> Env tm vars ->
                 Binder (tm (reverseOnto vars ns))
getBinderUnder {idx = Z} {vars = v :: vs} ns First (b :: env)
    = rewrite revOnto vs (v :: ns) in map (weakenNs (reverse (v :: ns))) b
getBinderUnder {idx = S k} {vars = v :: vs} ns (Later lp) (b :: env)
    = getBinderUnder (v :: ns) lp env

export
getBinder : Weaken tm =>
            {vars : _} -> {idx : Nat} ->
            (0 p : IsVar x idx vars) -> Env tm vars -> Binder (tm vars)
getBinder el env = getBinderUnder [] el env

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

 
