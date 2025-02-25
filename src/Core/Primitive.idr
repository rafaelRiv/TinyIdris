module Core.Primitive

public export
data Constant = BI Integer

export
Show Constant where
  show (BI x) = show x
