module Core.Core

export
data Error : Type where
  MsgError : String -> Error

public export
record Core t where
  constructor MkCore
  runCore : IO (Either Error t)
  
