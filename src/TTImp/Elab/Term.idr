module TTImp.Elab.Term

import Core.Core
import Core.Context
import Core.Env
import Core.Normalise
import Core.TT
import Core.UnifyState

import TTImp.TTImp

checkExp : {vars : _} ->
           {auto c : Ref Ctxt Defs} ->
           {auto u : Ref UST UState} ->
           Env Term vars ->
           (term : Term vars) ->
           (got : Glued vars) ->
           (expected : Maybe (Glued vars)) ->
           Core (Term vars, Glued vars)
