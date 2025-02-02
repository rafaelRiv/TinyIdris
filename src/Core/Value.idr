module Core.Value

import Core.TT

public export
data NF : List Name -> Type where
  NType : NF vars
