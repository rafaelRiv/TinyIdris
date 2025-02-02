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
data NameType : Type where
  Func : NameType
  Bound : NameType
  DataCon : (tag : Int) -> (arity : Name) -> NameType
  TyCon : (tag : Int) -> (arity : Name) -> NameType

public export
data IsVar : Name -> Nat -> List Name -> Type where
  First : IsVar n Z (n :: ns)
  Later : IsVar n i ns -> IsVar n (S i) (m :: ns)

public export 
data PiInfo : Type where
  Implicit : PiInfo
  Explicit : PiInfo

public export
Show PiInfo where
  show Implicit = "Implicit"
  show Explicit = "Explicit"

public export
data Binder : Type -> Type where
  Lam : PiInfo -> ty -> Binder ty
  Pi : PiInfo -> ty -> Binder ty

  PVar : ty -> Binder ty
  PVTy : ty -> Binder ty

public export
data Term : List Name -> Type where
  Local : (idx : Nat) ->
          (0 p : IsVar name idx vars) ->
          Term vars
  Ref : NameType -> Name -> Term vars
  Meta : Name -> List (Term vars) -> Term vars
  Bind : (x : Name) ->
         Binder (Term vars) ->
         (scope : Term (x :: vars)) ->
         Term vars
  App : Term vars -> Term vars -> Term vars
  TType : Term vars
  Erased : Term vars


