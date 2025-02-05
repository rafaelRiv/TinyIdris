module TTImp.ProcessDecl

import Core.Context
import Core.Core
import Core.TT
import Core.UnifyState

import TTImp.ProcessType

import TTImp.TTImp

export
processDecl : {auto c : Ref Ctxt Defs} ->
              {auto u : Ref UST UState} ->
              ImpDecl -> Core ()
processDecl (IClaim (MkImpTy n ty)) = processType n ty
processDecl (IData ddef) = pure ()
processDecl (IDef x xs) = pure ()
