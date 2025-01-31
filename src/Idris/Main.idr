module Idris.Main

import Parser.Source
import TTImp.Parser
import Core.Core
import System

runMain : Core ()
runMain = pure ()

main : IO ()
main = do 
    [_,fname] <- getArgs
        | _ => putStrLn "Usage: tinyidris <filename>"
    parseFile fname (prog fname)
    coreRun runMain
            (\err => printLn err)
            pure

