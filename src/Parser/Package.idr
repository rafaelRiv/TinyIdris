module Parser.Package

import Parser.Lexer.Package
import System.File

runParser : (str: String) -> IO ()
runParser str = lex str

export parseFile : (fn : String) -> IO ()
parseFile fn = do
    Right str <- readFile fn
        | Left err => putStrLn $ show err
    runParser str
