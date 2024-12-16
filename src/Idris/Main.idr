module Idris.Main

import Parser.Source
import System

main : IO ()
main = do 
    [_,fname] <- getArgs
        | _ => putStrLn "Usage: tinyidris <filename>"
    parseFile fname

