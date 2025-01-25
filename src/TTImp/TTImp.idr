module TTImp.TTImp

import Core.TT

public export
data RawImp : Type where
  IVar : Name -> RawImp 
  IPi : PiInfo -> Maybe Name ->
      (argTy : RawImp) -> (retTy : RawImp) -> RawImp
  ILam : PiInfo -> Maybe Name ->
      (argTy : RawImp) -> (scope : RawImp) -> RawImp
  IPatvar : Name -> (ty : RawImp) -> (scope : RawImp) -> RawImp
  IApp : RawImp -> RawImp -> RawImp
  Implicit : RawImp
  IType : RawImp


public export
data ImpTy : Type where
  MkImpTy : (n : Name) -> (ty : RawImp) -> ImpTy

public export
data ImpClause : Type where
  MkImpClause : (lhs : RawImp) -> (rhs : RawImp) -> ImpClause

public export
data ImpData : Type where
  MkImpData : (n : Name) -> 
              (tycon : RawImp) ->
              (datacons : List ImpTy) ->
              ImpData

public export
  data ImpDecl : Type where
    IClaim : ImpTy -> ImpDecl
    IData : ImpData -> ImpDecl
    IDef : Name -> List ImpClause -> ImpDecl

public export
Show RawImp where
  show IType = "IType"
  show Implicit = "Implicit"
  show (IVar name) = "Var " ++ show name
  show (IPi info name arg scope) = "IPi " ++ show info ++ " " ++ show arg ++ " " ++ " " ++ show scope
  show _ = "Not yet implemented"

public export
Show ImpTy where
  show (MkImpTy name imp) = show name ++ ":"  ++ show imp

public export
Show ImpData where
  show (MkImpData name tycon datacons) = show name ++ " "  ++ show tycon ++ " " ++ show datacons




