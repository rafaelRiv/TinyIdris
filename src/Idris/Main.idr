module Idris.Main

import Parser.Source

import System
import System.File

main : IO ()
main = do 
    [_,fname] <- getArgs
        | _ => putStrLn "Usage: tinyidris <filename>"
    Right str <- readFile fname
        | Left err => putStrLn $ show err
    pure ()

