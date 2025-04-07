module TTImp.ProcessDef

import Core.Context
import Core.Core
import Core.Env
import Core.Normalise
import Core.TT
import Core.UnifyState
import Core.CaseTree
import Core.CaseBuilder

import TTImp.Elab.Term
import TTImp.TTImp


getRHSEnv : {vars : _} ->
            Env Term vars -> Term vars -> Term vars ->
            Core (vars' ** (Env Term vars', Term vars', Term vars'))
-- The names have to match here, and if type checking is implemented correctly
-- they will, but we don't have a way to express that! So we need to check.
getRHSEnv env (Bind n (PVar ty) sc) (Bind n' (PVTy _) scty) with (nameEq n n')
  getRHSEnv env (Bind n (PVar ty) sc) (Bind n' (PVTy _) scty) | Nothing
      = throw (GenericMsg "Can't happen: names don't match in getRHSEnv")
  getRHSEnv env (Bind n (PVar ty) sc) (Bind n (PVTy _) scty) | (Just Refl)
      = getRHSEnv (PVar ty :: env) sc scty
getRHSEnv env lhs ty = pure (vars ** (env, lhs, ty))

processClause : {auto c : Ref Ctxt Defs} ->
                {auto u : Ref UST UState} ->
                ImpClause -> Core Clause
processClause (PatClause lhs rhs) 
  = do
      coreLift $ printLn lhs
      (lhstm, lhsty) <- checkTerm [] lhs Nothing
      (vars ** (env, lhsenv, rhsexp)) <-
        getRHSEnv [] lhstm !(getTerm lhsty)
      (rhstm, rhsty) <- checkTerm env rhs (Just (gnf env rhsexp))
      pure (MkClause env lhsenv rhstm)


export
processDef : {auto c : Ref Ctxt Defs} ->
             {auto u : Ref UST UState} ->
             Name -> List ImpClause -> Core ()
processDef n clauses
  = do
      defs <- get Ctxt
      Just gdef <- lookupDef n defs
        | Nothing => throw (GenericMsg ("No type declaration for " ++ show n))
      chkcs <- traverse processClause clauses

      -- Now we have all the clauses, make a case tree
      (args ** tree) <- getPMDef n (type gdef) chkcs

      -- Update the definition with the compiled tree
      updateDef n ({ definition := PMDef args tree })

      coreLift $ putStrLn "Global defs after processDef : \n"
      defs <- get Ctxt
      coreLift $ printLn defs

      coreLift $ putStrLn $ "Processed " ++ show n ++ "\n\n"
