module Core.Unify

import Core.Core
import Core.Context
import Core.Env
import Core.TT
import Core.UnifyState
import Core.Value

tm : List Name -> Type

export
unify : {vars : _} ->
        {auto c : Ref Ctxt Defs} ->
        {auto u : Ref UST UState} ->
        Env Term vars ->
        NF vars -> NF vars ->
        Core ()
unify _ _ _ = pure ()
