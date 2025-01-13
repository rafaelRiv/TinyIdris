module Core.TT

public export
data Name : Type where
  UN : String -> Name -- user written name
  MN : String -> Int -> Name

public export 
data PiInfo : Type where
  Implicit : PiInfo
  Explicit : PiInfo

