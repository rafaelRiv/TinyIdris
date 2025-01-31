module Idris.Main

import Parser.Source
import TTImp.Parser
import Core.Core
import System

coreRun' : IO (Either Error String)
coreRun' = pure (Right "Sucess")

main : IO ()
main = do 
    [_,fname] <- getArgs
        | _ => putStrLn "Usage: tinyidris <filename>"
    let core = MkCore coreRun'
    parseFile fname (prog fname)
    n <- core.runCore
    pure ()

