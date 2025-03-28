module Core.CaseTree

import Core.TT

mutual
  public export
  data CaseTree : List Name -> Type where
       -- case x return scTy of { p1 => e1 ; ... }
       Case : {name, vars : _} ->
              (idx : Nat) ->
              (0 p : IsVar name idx vars) ->
              (scTy : Term vars) -> List (CaseAlt vars) ->
              CaseTree vars
       -- RHS: no need for further inspection
       STerm : Term vars -> CaseTree vars
       -- error from a partial match
       Unmatched : (msg : String) -> CaseTree vars
       -- Absurd context
       Impossible : CaseTree vars

  -- Case alternatives. Unlike arbitrary patterns, they can be at most
  -- one constructor deep.
  -- Idris2 also needs cases for 'Delay' and primitives.
  public export
  data CaseAlt : List Name -> Type where
       -- Constructor for a data type; bind the arguments and subterms.
       ConCase : Name -> (tag : Int) -> (args : List Name) ->
                 CaseTree (args ++ vars) -> CaseAlt vars
       -- Catch-all case
       DefaultCase : CaseTree vars -> CaseAlt vars

-- Patterns, which arise from LHS expressions, and are converted to
-- case trees
public export
data Pat : Type where
     PCon : Name -> (tag : Int) -> (arity : Nat) ->
            List Pat -> Pat
     PLoc : Name -> Pat
     PUnmatchable : Term [] -> Pat
