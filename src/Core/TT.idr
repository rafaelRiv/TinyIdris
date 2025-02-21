module Core.TT

import Decidable.Equality

public export
data Name : Type where
  UN : String -> Name -- user written name
  MN : String -> Int -> Name

export
nameEq : (x : Name) -> (y : Name) -> Maybe (x = y)
nameEq (UN x) (UN y) with (decEq x y)
  nameEq (UN y) (UN y) | (Yes Refl) = Just Refl
  nameEq (UN x) (UN y) | (No contra) = Nothing
nameEq (MN x t) (MN x' t') with (decEq x x')
  nameEq (MN x t) (MN x t') | (Yes Refl) with (decEq t t')
    nameEq (MN x t) (MN x t) | (Yes Refl) | (Yes Refl) = Just Refl
    nameEq (MN x t) (MN x t') | (Yes Refl) | (No contra) = Nothing
  nameEq (MN x t) (MN x' t') | (No contra) = Nothing
nameEq _ _ = Nothing

export
Eq Name where
  (==) (UN x) (UN y) = x == y
  (==) (MN x i) (MN y j) = i == j && x == y
  (==) _ _ = False

nameTag : Name -> Int
nameTag (UN _) = 0
nameTag (MN _ _) = 1

export
Ord Name where
  compare (UN x) (UN y) = compare x y
  compare (MN x i) (MN y j)
      = case compare x y of
             EQ => compare i j
             t => t
  compare x y = compare (nameTag x) (nameTag y)

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
data NVar : Name -> List Name -> Type where
     MkNVar : {i : Nat} -> (0 p : IsVar n i vars) -> NVar n vars

export
weakenNVar : (ns : List Name) ->
             {idx : Nat} -> (0 p : IsVar name idx inner) ->
             NVar name (ns ++ inner)
weakenNVar [] x = MkNVar x
weakenNVar (y :: xs) x
   = let MkNVar x' = weakenNVar xs x in
         MkNVar (Later x')

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

export
binderType : Binder tm -> tm
binderType (Lam x ty) = ty
binderType (Pi x ty) = ty
binderType (PVar ty) = ty
binderType (PVTy ty) = ty

export
Functor Binder where
  map func (Lam x ty) = Lam x (func ty)
  map func (Pi x ty) = Pi x (func ty)
  map func (PVar ty) = PVar (func ty)
  map func (PVTy ty) = PVTy (func ty)

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

public export
Show (Term names) where
 {- 
  show (IVar name) = "Var " ++ show name
  show (IPi info name arg scope) = "IPi " ++ show info ++ " " ++ show arg ++ " " ++ " " ++ show scope
  show (IApp imp imp') = "IApp " ++ show imp ++ " " ++ show imp' -}
  show TType = "TType"
  show Erased = "Erased"
  show _ = "Not yet implemented"

public export
interface Weaken (0 tm : List Name -> Type) where
  weaken : {n,vars : _} -> tm vars -> tm (n :: vars)
  weakenNs : {vars : _} -> (ns : List Name) -> tm vars -> tm (ns ++ vars)

  weakenNs [] t = t
  weakenNs (n :: ns) t = weaken (weakenNs ns t)

  weaken = weakenNs [_]

export
insertNVarNames : {outer, ns : _} ->
                  (idx : Nat) ->
                  (0 p : IsVar name idx (outer ++ inner)) ->
                  NVar name (outer ++ (ns ++ inner))
insertNVarNames {ns} {outer = []} idx prf = weakenNVar ns prf
insertNVarNames {outer = (y :: xs)} Z First = MkNVar First
insertNVarNames {ns} {outer = (y :: xs)} (S i) (Later x)
    = let MkNVar prf = insertNVarNames {ns} i x in
          MkNVar (Later prf)

export
insertNames : {outer, inner : _} ->
              (ns : List Name) -> Term (outer ++ inner) ->
              Term (outer ++ (ns ++ inner))
insertNames ns (Local idx prf)
    = let MkNVar prf' = insertNVarNames {ns} idx prf in
          Local _ prf'
insertNames ns (Ref nt name) = Ref nt name
insertNames ns (Meta name args)
    = Meta name (map (insertNames ns) args)
insertNames {outer} {inner} ns (Bind x b scope)
    = Bind x (assert_total (map (insertNames ns) b))
           (insertNames {outer = x :: outer} {inner} ns scope)
insertNames ns (App fn arg)
    = App (insertNames ns fn) (insertNames ns arg)
insertNames ns Erased = Erased
insertNames ns TType = TType

export
Weaken Term where
  weakenNs ns tm = insertNames {outer = []} ns tm




