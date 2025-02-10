module Core.Normalise

import Core.Context
import Core.Core
import Core.Env
import Core.TT
import Core.Value

public export
data Glued : List Name -> Type where
  MkGlue : Core (Term vars) ->
           (Ref Ctxt Defs -> Core (NF vars)) -> Glued vars

export
toClosure : Env Term outer -> Term outer -> Closure outer
toClosure env tm = MkClosure [] env tm

export
nf : {vars : _} ->
     Defs -> Env Term vars -> Term vars -> Core (NF vars)
nf defs env tm = pure NErased-- eval defs env [] tm []

export
gnf : {vars : _} ->
      Env Term vars -> Term vars -> Glued vars
gnf env tm
    = MkGlue (pure tm)
             (\c => do defs <- get Ctxt
                       nf defs env tm)

export
gType : Glued vars
gType = MkGlue (pure TType) (const (pure NType))

export
getValArity : {vars : _} ->
              Defs -> Env Term vars -> NF vars -> Core Nat
getValArity defs env (NBind x (Pi _ _) sc)
    = pure (S !(getValArity defs env !(sc defs (toClosure env Erased))))
getValArity defs env val = pure 0

export
getArity : {vars : _} ->
           Defs -> Env Term vars -> Term vars -> Core Nat
getArity defs env tm = pure 0 -- getValArity defs env !(nf defs env tm)
