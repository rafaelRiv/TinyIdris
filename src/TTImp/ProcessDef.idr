module TTImp.ProcessDef

import Core.Context
import Core.Core
import Core.Env
import Core.Normalise
import Core.TT
import Core.UnifyState

import TTImp.Elab.Term
import TTImp.TTImp

export
processDef : {auto c : Ref Ctxt Defs} ->
             {auto u : Ref UST UState} ->
             Name -> List ImpClause -> Core ()
processDef n ty 
  = do
      coreLift $ putStrLn $ "Processed " ++ show n ++ "\n\n"
