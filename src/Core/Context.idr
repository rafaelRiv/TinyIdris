module Core.Context

import Core.Core
import Core.TT
import Core.CaseTree

import Data.SortedMap

import System

public export
data Def : Type where
    None : Def -- Not yet defined
    PMDef : (args : List Name) -> (treeCT : CaseTree args) ->
            Def -- Ordinary function definition
    DCon : (tag : Int) -> (arity : Nat) -> Def -- data constructor
    TCon : (tag : Int) -> (arity : Nat) -> Def
    Hole : Def
    Guess : (guess : Term []) ->
            (constraints : List Int) -> Def -- unification constraints

export
Show Def where
  show (TCon tag arity) = "TCon"
  show (DCon tag arity) = "DCon"
  show _ = "Not implemented yet"

public export
record GlobalDef where
  constructor MkGlobalDef
  type : Term []
  definition : Def

export
Show GlobalDef where
  show gd = "{ type : " ++ show gd.type ++ ", definition : " ++ show gd.definition

export
newDef : Term [] -> Def -> GlobalDef
newDef ty d = MkGlobalDef ty d

export
Defs : Type
Defs = SortedMap Name GlobalDef

export
Show Defs where
  show defs = show $ map show defs

export
lookupDef : Name -> Defs -> Core (Maybe GlobalDef)
lookupDef n defs = pure (SortedMap.lookup n defs)

export
data Ctxt : Type where

export
initDefs : Core Defs
initDefs = pure empty

-- Add (or replace) a definition

export
addDef : {auto c : Ref Ctxt Defs} ->
         Name  -> GlobalDef -> Core ()
addDef n d
  = do defs <- get Ctxt
       put Ctxt (insert n d defs)

export
updateDef : {auto c : Ref Ctxt Defs} ->
            Name -> (GlobalDef -> GlobalDef) -> Core ()
updateDef n upd
    = do defs <- get Ctxt
         Just gdef <- lookupDef n defs
            | Nothing => throw (UndefinedName n)
         addDef n (upd gdef)
