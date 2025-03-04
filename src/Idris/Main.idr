module Idris.Main

import Parser.Source
import TTImp.Parser
import TTImp.ProcessDecl
import Core.Core
import Core.UnifyState
import Core.Context
import System

runMain : List ImpDecl -> Core ()
runMain decls = do
  c <- newRef Ctxt !initDefs
  u <- newRef UST initUState
  traverse_ (\imp => do 
      processDecl imp
      coreLift $ putStrLn "Global defs : \n"
      defs <- get Ctxt
      coreLift $ printLn defs
      coreLift $ putStrLn "\n"
    ) decls

main : IO ()
main = do 
    [_,fname] <- getArgs
        | _ => putStrLn "Usage: tinyidris <filename>"
    Right decls <- parseFile fname (prog fname)
      | Left err => printLn err
    
    putStrLn "Decs : \n"
    printLn decls
    putStrLn ""

    coreRun (runMain decls)
            (\err => printLn err)
            pure

