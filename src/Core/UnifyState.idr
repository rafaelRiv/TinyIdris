module Core.UnifyState

import Core.TT
import Core.Env

import Data.SortedMap
import Data.SortedSet

public export
data Constraint : Type where
  MkConstraint : {vars : _} ->
                 (env : Env Term vars) ->
                 (x : Term vars) -> (y : Term vars) ->
                 Constraint

  MkSeqConstraint : {vars : _} ->
                    (env : Env Term vars) ->
                    (xs : List (Term vars)) ->
                    (yx : List (Term vars)) ->
                    Constraint
  Resolved : Constraint

public export
record UState where
  constructor MkUState
  holes: SortedSet Name
  guesses : SortedSet Name
  constraints : SortedMap Int Constraint -- map for finding constraints by ID
  nextName : Int
  nextConstraint : Int

export
data UST : Type where
