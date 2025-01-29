module Core.Core

data Error : Type where
  MsgError : String -> Error

record Core t where
  constructor MkCore
  runCore : IO (Either Error t)
  
