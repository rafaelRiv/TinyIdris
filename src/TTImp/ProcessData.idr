module TTImp.ProcessData

import Core.Context
import Core.Core
import Core.Env
import Core.Normalise
import Core.TT
import Core.UnifyState

import TTImp.Elab.Term
import TTImp.TTImp

export
processData : {auto c : Ref Ctxt Defs} ->
              {auto u : Ref UST UState} ->
              ImpData -> Core ()
processData (MkImpData n tycon datacons) = do
  -- Check tycon
  checkTerm [] tycon (Just gType)

  {- TODO

  * Get arity
  * Add def n
  * check datacons
  * add cons to def

  -}


