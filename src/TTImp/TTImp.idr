module TTImp.TTImp

import Core.TT
import Core.Primitive
import Data.Vect

vect1 : Vect (plus 2 2) Int

public export
data RawImp : Type where
  IVar : Name -> RawImp 
  IPrim : Constant -> RawImp 
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
  PatClause : (lhs : RawImp) -> (rhs : RawImp) -> ImpClause

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

export
apply : RawImp -> List RawImp -> RawImp
apply f [] = f
apply f (x :: xs) = apply (IApp f x) xs

public export
Show RawImp where
  show IType = "IType"
  show Implicit = "Implicit"
  show (IVar name) = "Var " ++ show name
  show (IPrim prim) = "Prim " ++ show prim
  show (IPi info name arg retTy) = "IPi " ++ show info ++ " " ++ show name ++ " " ++ show arg ++ " " ++ show retTy
  show (IApp imp imp') = "IApp : [" ++ show imp ++ "][" ++ show imp' ++ "]"
  show _ = "Not yet implemented"

public export
Show ImpTy where
  show (MkImpTy name imp) = show name ++ ":"  ++ show imp

public export
Show ImpData where
  show (MkImpData name tycon datacons) = show name ++ " "  ++ show tycon ++ " " ++ show datacons

public export
Show ImpDecl where
  show (IClaim impTy) = "IClaim " ++ show impTy
  show (IData iDef) = "IData " ++ show iDef
  show _ = "Not yet implemented"




