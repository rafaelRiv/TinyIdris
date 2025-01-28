module Idris.Main

import Parser.Source
import TTImp.Parser
import System

main : IO ()
main = do 
    [_,fname] <- getArgs
        | _ => putStrLn "Usage: tinyidris <filename>"
    parseFile fname (prog fname)

