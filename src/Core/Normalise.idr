module Core.Normalise

import Core.Context
import Core.Core
import Core.TT
import Core.Value

public export
data Glued : List Name -> Type where
  MkGlue : Core (Term vars) ->
           (Ref Ctxt Defs -> Core (NF vars)) -> Glued vars
