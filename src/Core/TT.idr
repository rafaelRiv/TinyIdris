module Core.TT

public export
data Name : Type where
  UN : String -> Name -- user written name
  MN : String -> Int -> Name

export
Eq Name where
  (==) (UN x) (UN y) = x == y
  (==) (MN x i) (MN y j) = i == j && x == y
  (==) _ _ = False

public export
Show Name where
  show (UN name) = name
  show (MN name _) = name

public export 
data PiInfo : Type where
  Implicit : PiInfo
  Explicit : PiInfo

public export
Show PiInfo where
  show Implicit = "Implicit"
  show Explicit = "Explicit"

