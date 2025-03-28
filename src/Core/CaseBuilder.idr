module Core.CaseBuilder

import Core.CaseTree
import Core.Context
import Core.Core
import Core.Env
import Core.Normalise
import Core.TT
import Core.Value

import Data.List
import Data.String

data ArgType : List Name -> Type where
     Known : (ty : Term vars) -> ArgType vars -- arg has type 'ty'
     Stuck : (fty : Term vars) -> ArgType vars
         -- ^ arg will have argument type of 'fty' when we know enough to
         -- calculate it
     Unknown : ArgType vars
         -- arg's type is not yet known due to a previously stuck argument

{ns : _} -> Show (ArgType ns) where
  show (Known t) = "Known " ++ show t
  show (Stuck t) = "Stuck " ++ show t
  show Unknown = "Unknown"

record PatInfo (pvar : Name) (vars : List Name) where
  constructor MkInfo
  {idx : Nat}
  {name : Name}
  pat : Pat
  0 loc : IsVar name idx vars
  argType : ArgType vars -- Type of the argument being inspected (i.e.
                         -- *not* refined by this particular pattern)

{-
NamedPats is a list of patterns on the LHS of a clause. Each entry in
the list gives a pattern, and a proof that there is a variable we can
inspect to see if it matches the pattern.

A definition consists of a list of clauses, which are a 'NamedPats' and
a term on the RHS. There is an assumption that corresponding positions in
NamedPats always have the same 'Elem' proof, though this isn't expressed in
a type anywhere.
-}

data NamedPats : List Name -> -- pattern variables still to process
                 List Name -> -- the pattern variables still to process,
                              -- in order
                 Type where
     Nil : NamedPats vars []
     (::) : PatInfo pvar vars ->
            -- ^ a pattern, where its variable appears in the vars list,
            -- and its type. The type has no variable names; any names it
            -- refers to are explicit
            NamedPats vars ns -> NamedPats vars (pvar :: ns)

data PatClause : (vars : List Name) -> (todo : List Name) -> Type where
     MkPatClause : List Name -> -- names matched so far (from original lhs)
                   NamedPats vars todo ->
                   (rhs : Term vars) -> PatClause vars todo

data Partitions : List (PatClause vars todo) -> Type where
     ConClauses : {todo, vars, ps : _} ->
                  (cs : List (PatClause vars todo)) ->
                  Partitions ps -> Partitions (cs ++ ps)
     VarClauses : {todo, vars, ps : _} ->
                  (vs : List (PatClause vars todo)) ->
                  Partitions ps -> Partitions (vs ++ ps)
     NoClauses : Partitions []

data ClauseType = ConClause | VarClause


mkPatClause : {auto c : Ref Ctxt Defs} ->
              Name ->
              (args : List Name) -> Term [] ->
              (List Pat, Term []) ->
              Core (PatClause args args)
mkPatClause fn args ty (ps, rhs)
    = maybe (throw (CaseCompile fn DifferingArgNumbers))
            (\eq =>
               do defs <- get Ctxt
                  nty <- nf defs [] ty
                  ns <- mkNames args ps eq (Just nty)
                  pure (MkPatClause [] ns
                          (rewrite sym (appendNilRightNeutral args) in
                                   (weakenNs args rhs))))
            (checkLengthMatch args ps)
  where
    mkNames : (vars : List Name) -> (ps : List Pat) ->
              LengthMatch vars ps -> Maybe (NF []) ->
              Core (NamedPats vars vars)
    mkNames [] [] NilMatch fty = pure []
    mkNames (arg :: args) (p :: ps) (ConsMatch eq) fty
        = do defs <- get Ctxt
             empty <- clearDefs defs
             fa_tys <- the (Core (Maybe _, ArgType _)) $
                case fty of
                     Nothing => pure (Nothing, CaseBuilder.Unknown)
                     Just (NBind _ (Pi _ farg) fsc) =>
                        pure (Just !(fsc defs (toClosure [] (Ref Bound arg))),
                                Known (embed {more = arg :: args}
                                          !(quote empty [] farg)))
                     Just t =>
                        pure (Nothing,
                                Stuck (embed {more = arg :: args}
                                        !(quote empty [] t)))
             pure (MkInfo p First (Builtin.snd fa_tys)
                      :: weaken !(mkNames args ps eq (Builtin.fst fa_tys)))

export
patCompile : {auto c : Ref Ctxt Defs} ->
             Name ->
             Term [] -> List (List Pat, Term []) ->
             Maybe (CaseTree []) ->
             Core (args ** CaseTree args)
patCompile fn ty [] def
    = maybe (pure ([] ** Unmatched "No definition"))
            (\e => pure ([] ** e))
            def
patCompile fn ty (p :: ps) def
    = do let ns = getNames 0 (fst p)
         pats <- mkPatClausesFrom ns (p :: ps)
         i <- newRef PName (the Int 0)
         cases <- match fn pats
                        (rewrite sym (appendNilRightNeutral ns) in
                                 map (TT.weakenNs ns) def)
         pure (_ ** cases)
  where
    mkPatClausesFrom : (args : List Name) ->
                       List (List Pat, Term []) ->
                       Core (List (PatClause args args))
    mkPatClausesFrom ns [] = pure []
    mkPatClausesFrom ns (p :: ps)
        = do p' <- mkPatClause fn ns ty p
             ps' <- mkPatClausesFrom ns ps
             pure (p' :: ps')

    getNames : Int -> List Pat -> List Name
    getNames i [] = []
    getNames i (x :: xs) = MN "arg" i :: getNames (i + 1) xs

toPatClause : {auto c : Ref Ctxt Defs} ->
              Name -> (Term [], Term []) ->
              Core (List Pat, Term [])
toPatClause n (lhs, rhs)
    = case getFnArgs lhs of
           (Ref Func fn, args)
              => do defs <- get Ctxt
                    if n == fn
                       then pure (map argToPat args, rhs)
                       else throw (GenericMsg ("Wrong function name in pattern LHS " ++ show (n, fn)))
           (f, args) => throw (GenericMsg "Not a function name in pattern LHS")

-- Assumption (given 'Term []) is that the pattern variables are
-- explicitly named. We'll assign de Bruijn indices when we're done, and
-- the names of the top level variables we created are returned in 'args'
export
simpleCase : {auto c : Ref Ctxt Defs} ->
             Name -> Term [] -> (def : Maybe (CaseTree [])) ->
             (clauses : List (Term [], Term [])) ->
             Core (args ** CaseTree args)
simpleCase fn ty def clauses
    = do ps <- traverse (toPatClause fn) clauses
         defs <- get Ctxt
         patCompile fn ty ps def

-- Converting a list of pattern clauses to a case tree.
-- Returns the generated argument names for the top level arguments,
-- and a case tree which deconstructs them
export
getPMDef : {auto c : Ref Ctxt Defs} ->
           Name -> -- name of function we're compiling
           Term [] -> -- function's type
           List Clause -> -- input clauses
           Core (args ** CaseTree args)
getPMDef fn ty []
    = do defs <- get Ctxt
         pure (!(getArgs 0 !(nf defs [] ty)) ** (Unmatched "No clauses"))
  where
    getArgs : Int -> NF [] -> Core (List Name)
    getArgs i (NBind x (Pi _ _) sc)
        = do defs <- get Ctxt
             sc' <- sc defs (toClosure [] Erased)
             pure (MN "arg" i :: !(getArgs i sc'))
    getArgs i _ = pure []
getPMDef fn ty clauses
    = do defs <- get Ctxt
         let cs = map (toClosed defs) (labelPat 0 clauses)
         (_ ** t) <- simpleCase fn ty Nothing cs
         pure (_ ** t)
  where
    labelPat : Int -> List a -> List (String, a)
    labelPat i [] = []
    labelPat i (x :: xs) = ("pat" ++ show i ++ ":", x) :: labelPat (i + 1) xs

    mkSubstEnv : Int -> String -> Env Term vars -> SubstEnv vars []
    mkSubstEnv i pname [] = Nil
    mkSubstEnv i pname (v :: vs)
       = Ref Bound (MN pname i) :: mkSubstEnv (i + 1) pname vs

    close : {vars : _} ->
            Env Term vars -> String -> Term vars -> Term []
    close {vars} env pname tm
        = substs (mkSubstEnv 0 pname env)
              (rewrite appendNilRightNeutral vars in tm)

    toClosed : Defs -> (String, Clause) -> (Term [], Term [])
    toClosed defs (pname, MkClause env lhs rhs)
          = (close env pname lhs, close env pname rhs)
