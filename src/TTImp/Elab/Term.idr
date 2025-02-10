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
checkExp env term got Nothing = pure (term, got)
checkExp env term got (Just exp) = pure (term,exp)

export
checkTerm : {vars : _} ->
            {auto c : Ref Ctxt Defs} ->
            {auto u : Ref UST UState} ->
            Env Term vars -> RawImp -> Maybe (Glued vars) ->
            Core (Term vars, Glued vars)
checkTerm env (IVar n) exp =
    case defined n env of
         (Just (MkIsDefined p)) => 
            let binder = getBinder p env in
                checkExp env (Local _ p)
                              (gnf env (binderType binder))
                              exp
         Nothing =>
            do defs <- get Ctxt
               Just gdef <- lookupDef n defs
                  | Nothing => throw (UndefinedName n)
               pure (TType,gType)
checkTerm env IType exp = pure (TType, gType)
checkTerm env imp exp = pure (TType, gType)
