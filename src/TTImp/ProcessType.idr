module TTImp.ProcessType

import Core.Context
import Core.Core
import Core.TT
import Core.UnifyState
import TTImp.TTImp

export
processType : {auto c : Ref Ctxt Defs} ->
              {auto u : Ref UST UState} ->
              Name -> RawImp -> Core ()
processType n ty = do
  coreLift $ printLn n
  coreLift $ printLn ty
  pure ()
