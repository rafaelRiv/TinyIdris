module TTImp.ProcessData

import Core.Context
import Core.Core
import Core.Env
import Core.Normalise
import Core.TT
import Core.UnifyState

import TTImp.Elab.Term
import TTImp.TTImp

import Data.List

export
processCon : {auto c : Ref Ctxt Defs} ->
             {auto u : Ref UST UState} ->
             ImpTy -> Core (Name, Term [])
processCon (MkImpTy n ty)
  = do (tychk, _) <- checkTerm [] ty (Just gType)
       pure (n, tychk)

export
processData : {auto c : Ref Ctxt Defs} ->
              {auto u : Ref UST UState} ->
              ImpData -> Core ()
processData (MkImpData n tycon datacons) =
   do (tychk, _) <- checkTerm [] tycon (Just gType)
      -- Add it to the context before checking data constructors
      -- Exercise: We should also check whether it't already defined!
      defs <- get Ctxt
      arity <- getArity defs [] tychk
      addDef n (newDef tychk (TCon 0 arity))
      chkcons <- traverse processCon datacons
      
      defs <- get Ctxt
      traverse_ (\(i, (cn,ty)) =>
                  do carity <- getArity defs [] ty
                     addDef cn (newDef ty (DCon (cast i) carity)))
                (zip [0..(length chkcons)] chkcons)
    
      coreLift $ putStrLn "Global defs after processData : \n"
      defs <- get Ctxt
      coreLift $ printLn defs
      coreLift $ putStrLn "\n"

      coreLift $ putStrLn $ "Processed " ++ show n
      coreLift $ putStrLn "\n"



