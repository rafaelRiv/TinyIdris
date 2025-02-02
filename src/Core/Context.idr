module Core.Context

import Core.TT

import Data.SortedMap

public export
data Def : Type where
  None : Def

public export
record GlobalDef where
  constructor MkGlobalDef
  type : Term []
  definition : Def

export
Defs : Type
Defs = SortedMap Name GlobalDef

export
data Ctxt : Type where
