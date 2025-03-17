module Core.Normalise

import Core.Context
import Core.Core
import Core.Env
import Core.TT
import Core.Value

public export
data Glued : List Name -> Type where
  MkGlue : Core (Term vars) ->
           (Ref Ctxt Defs -> Core (NF vars)) -> Glued vars

export
getTerm : Glued vars -> Core (Term vars)
getTerm (MkGlue tm _) = tm

export
getNF : {auto c : Ref Ctxt Defs} -> Glued vars -> Core (NF vars)
getNF {c} (MkGlue _ nf) = nf c

Stack : List Name -> Type
Stack vars = List (Closure vars)

export
toClosure : Env Term outer -> Term outer -> Closure outer
toClosure env tm = MkClosure [] env tm

parameters (defs : Defs)
  mutual
    eval : {free, vars : _} ->
          Env Term free -> LocalEnv free vars ->
          Term (vars ++ free) -> Stack free -> Core (NF free)
    eval env locs TType stk = pure NType
    eval env locs Erased stk = pure NErased
    eval env locs term stk = pure NErased

evalClosure : {free : _} -> Defs -> Closure free -> Core (NF free)
evalClosure defs (MkClosure locs env tm)
  = eval defs env locs tm []

export
nf : {vars : _} ->
     Defs -> Env Term vars -> Term vars -> Core (NF vars)
nf defs env tm = eval defs env [] tm []

export
gnf : {vars : _} ->
      Env Term vars -> Term vars -> Glued vars
gnf env tm
    = MkGlue (pure tm)
             (\c => do defs <- get Ctxt
                       nf defs env tm)

export
gType : Glued vars
gType = MkGlue (pure TType) (const (pure NType))

export
data QVar : Type where

public export
interface Quote (0 tm : List Name -> Type) where
    quote : {vars : _} ->
            Defs -> Env Term vars -> tm vars -> Core (Term vars)
    quoteGen : {vars : _} ->
               Ref QVar Int -> Defs -> Env Term vars ->
               tm vars -> Core (Term vars)

    quote defs env tm
        = do q <- newRef QVar 0
             quoteGen q defs env tm
export
genName : {auto q : Ref QVar Int} -> String -> Core Name
genName n
    = do i <- get QVar
         put QVar (i + 1)
         pure (MN n i)

mutual
  quoteArgs : {bound, free : _} ->
              Ref QVar Int -> Defs -> Bounds bound ->
              Env Term free -> List (Closure free) ->
              Core (List (Term (bound ++ free)))
  quoteArgs q defs bounds env [] = pure []
  quoteArgs q defs bounds env (a :: args)
      = pure $ (!(quoteGenNF q defs bounds env !(evalClosure defs a)) ::
                !(quoteArgs q defs bounds env args))

  quoteHead : {bound, free : _} ->
              Ref QVar Int -> Defs ->
              Bounds bound -> Env Term free -> NHead free ->
              Core (Term (bound ++ free))
  quoteHead {bound} q defs bounds env (NLocal _ prf)
      = let MkVar prf' = addLater bound prf in
            pure $ Local _ prf'
    where
      addLater : {idx : _} ->
                 (ys : List Name) -> (0 p : IsVar n idx xs) ->
                 Var (ys ++ xs)
      addLater [] isv = MkVar isv
      addLater (x :: xs) isv
          = let MkVar isv' = addLater xs isv in
                MkVar (Later isv')
  quoteHead q defs bounds env (NRef Bound (MN n i))
      = case findName bounds of
             Just (MkVar p) => pure $ Local _ (varExtend p)
             Nothing => pure $ Ref Bound (MN n i)
    where
      findName : Bounds bound' -> Maybe (Var bound')
      findName None = Nothing
      findName (Add x (MN n' i') ns)
          = if i == i' -- this uniquely identifies it, given how we
                       -- generated the names, and is a faster test!
               then Just (MkVar First)
               else do MkVar p <-findName ns
                       Just (MkVar (Later p))
      findName (Add x _ ns)
          = do MkVar p <- findName ns
               Just (MkVar (Later p))
  quoteHead q defs bounds env (NRef nt n) = pure $ Ref nt n
  quoteHead q defs bounds env (NMeta n args)
      = do args' <- quoteArgs q defs bounds env args
           pure $ Meta n args'

  quoteBinder : {bound, free : _} ->
                Ref QVar Int -> Defs -> Bounds bound ->
                Env Term free -> Binder (NF free) ->
                Core (Binder (Term (bound ++ free)))
  quoteBinder q defs bounds env (Lam p ty)
      = do ty' <- quoteGenNF q defs bounds env ty
           pure (Lam p ty')
  quoteBinder q defs bounds env (Pi p ty)
      = do ty' <- quoteGenNF q defs bounds env ty
           pure (Pi p ty')
  quoteBinder q defs bounds env (PVar ty)
      = do ty' <- quoteGenNF q defs bounds env ty
           pure (PVar ty')
  quoteBinder q defs bounds env (PVTy ty)
      = do ty' <- quoteGenNF q defs bounds env ty
           pure (PVTy ty')

  quoteGenNF : {bound, vars : _} ->
               Ref QVar Int ->
               Defs -> Bounds bound ->
               Env Term vars -> NF vars -> Core (Term (bound ++ vars))
  quoteGenNF q defs bound env (NBind n b sc)
      = do var <- genName "qv"
           sc' <- quoteGenNF q defs (Add n var bound) env
                       !(sc defs (toClosure env (Ref Bound var)))
           b' <- quoteBinder q defs bound env b
           pure (Bind n b' sc')
  {- quoteGenNF q defs bound env (NApp f args)
      = do f' <- quoteHead q defs bound env f
           args' <- quoteArgs q defs bound env args
           pure $ apply f' args'
  quoteGenNF q defs bound env (NDCon n t ar args)
      = do args' <- quoteArgs q defs bound env args
           pure $ apply (Ref (DataCon t ar) n) args'
  quoteGenNF q defs bound env (NTCon n t ar args)
      = do args' <- quoteArgs q defs bound env args
           pure $ apply (Ref (TyCon t ar) n) args' -}
  quoteGenNF q defs bound env NErased = pure Erased
  quoteGenNF q defs bound env NType = pure TType
  quoteGenNF q defs bound env _ = pure TType

export
Quote NF where
  quoteGen q defs env tm = quoteGenNF q defs None env tm

export
Quote Term where
  quoteGen q defs env tm = pure tm

export
Quote Closure where
  quoteGen q defs env c = quoteGen q defs env !(evalClosure defs c)

export
glueBack : {vars : _} ->
           Defs -> Env Term vars -> NF vars -> Glued vars
glueBack defs env nf
    = MkGlue (do empty <- clearDefs defs
                 quote empty env nf)
             (const (pure nf))

export
getValArity : {vars : _} ->
              Defs -> Env Term vars -> NF vars -> Core Nat
getValArity defs env (NBind x (Pi _ _) sc)
    = pure (S !(getValArity defs env !(sc defs (toClosure env Erased))))
getValArity defs env val = pure 0

export
getArity : {vars : _} ->
           Defs -> Env Term vars -> Term vars -> Core Nat
getArity defs env tm = pure 0 -- getValArity defs env !(nf defs env tm)
