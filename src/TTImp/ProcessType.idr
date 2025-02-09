module TTImp.ProcessType

import Core.Context
import Core.Core
import Core.Env
import Core.Normalise
import Core.TT
import Core.UnifyState

import TTImp.Elab.Term
import TTImp.TTImp

export
processType : {auto c : Ref Ctxt Defs} ->
              {auto u : Ref UST UState} ->
              Name -> RawImp -> Core ()
processType n ty 
  = do
      (tychk, _) <- checkTerm [] ty (Just gType)
      addDef n (newDef tychk None)
