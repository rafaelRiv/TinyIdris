module Core.TT

import Decidable.Equality

%default partial

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
  DataCon : (tag : Int) -> (arity : Nat) -> NameType
  TyCon : (tag : Int) -> (arity : Nat) -> NameType

export
Show NameType where
  show Func = "Func"
  show (DataCon t a) = "DataCon " ++ show (t, a)
  show (TyCon t a) = "TyCon " ++ show (t, a)
  show Bound = "Bound"

public export
data IsVar : Name -> Nat -> List Name -> Type where
  First : IsVar n Z (n :: ns)
  Later : IsVar n i ns -> IsVar n (S i) (m :: ns)

public export
data Var : List Name -> Type where
  MkVar : {i : Nat} -> (0 p : IsVar n i vars) -> Var vars

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

public export
Show ty => Show (Binder ty) where
  show (Lam piInfo ty) = "Lam"
  show (Pi piInfo ty) = "Pi " ++ show  piInfo ++ " " ++ show ty
  show _ = "Not Implemented"

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
  show (Local idx p) = "Local"
  show (Ref type name) = "Ref [ " ++ show type ++ ", " ++ show name ++ " ]" 
  show (Meta name terms) = "Meta " ++ show name
  show (App term term') = "App " ++ show term ++ " " ++ show term'
  show (Bind name binders scope) = "Binder " ++ show name ++ "," ++ assert_total(show binders) ++ "," ++ show scope
  show TType = "TType"
  show Erased = "Erased"

public export
interface Weaken (0 tm : List Name -> Type) where
  weaken : {n,vars : _} -> tm vars -> tm (n :: vars)
  weakenNs : {vars : _} -> (ns : List Name) -> tm vars -> tm (ns ++ vars)

  weakenNs [] t = t
  weakenNs (n :: ns) t = weaken (weakenNs ns t)

  weaken = weakenNs [_]

export
varExtend : IsVar x idx sx -> IsVar x idx (xs ++ ys)
varExtend p = believe_me p

export
embed : Term vars -> Term (vars ++ more)
embed tm = believe_me tm

export
getFnArgs : Term vars -> (Term vars, List (Term vars))
getFnArgs tm = getFA [] tm
  where
    getFA : List (Term vars) -> Term vars ->
            (Term vars, List (Term vars))
    getFA args (App f a) = getFA (a :: args) f
    getFA args tm = (tm, args)

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

namespace Bounds
  public export
  data Bounds : List Name -> Type where
       None : Bounds []
       Add : (x : Name) -> Name -> Bounds xs -> Bounds (x :: xs)

-- Substitute some explicit terms for names in a term, and remove those
-- names from the scope
namespace SubstEnv
  public export
  data SubstEnv : List Name -> List Name -> Type where
       Nil : SubstEnv [] vars
       (::) : Term vars ->
              SubstEnv ds vars -> SubstEnv (d :: ds) vars

  findDrop : {drop : _} -> {idx : Nat} -> (0 p : IsVar name idx (drop ++ vars)) ->
             SubstEnv drop vars -> Term vars
  findDrop {drop = []} var env = Local _ var
  findDrop {drop = x :: xs} First (tm :: env) = tm
  findDrop {drop = x :: xs} (Later p) (tm :: env)
      = findDrop p env

  find : {drop, vars, outer : _} -> {idx : Nat} ->
         (0 p : IsVar name idx (outer ++ (drop ++ vars))) ->
         SubstEnv drop vars ->
         Term (outer ++ vars)
  find {outer = []} var env = findDrop var env
  find {outer = x :: xs} First env = Local _ First
  find {outer = x :: xs} (Later p) env = weaken (find p env)

  substEnv : {drop, vars, outer : _} ->
             SubstEnv drop vars -> Term (outer ++ (drop ++ vars)) ->
             Term (outer ++ vars)
  substEnv env (Local _ prf)
      = find prf env
  substEnv env (Ref x name) = Ref x name
  substEnv env (Meta n xs)
      = Meta n (map (substEnv env) xs)
  substEnv {outer} env (Bind x b scope)
      = Bind x (map (substEnv env) b)
               (substEnv {outer = x :: outer} env scope)
  substEnv env (App fn arg)
      = App (substEnv env fn) (substEnv env arg)
  substEnv env Erased = Erased
  substEnv env TType = TType

  export
  substs : {drop, vars : _} ->
           SubstEnv drop vars -> Term (drop ++ vars) -> Term vars
  substs env tm = substEnv {outer = []} env tm

  export
  subst : {vars, x : _} -> Term vars -> Term (x :: vars) -> Term vars
  subst val tm = substEnv {outer = []} {drop = [_]} [val] tm




