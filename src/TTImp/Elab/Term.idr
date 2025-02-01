module TTImp.Elab.Term

import Core.Core
import Core.TT

import TTImp.TTImp

checkExp : {vars : _} ->
           {auto c : Ref Ctxt Defs} ->
           {auto u : Ref UST UState} ->
           Env Term vars ->
           (term : Term vars) ->
           (got : Glued vars) ->
           (expected : Maybe (Glued vars)) ->
           Core (Term vars, Glued vars)
