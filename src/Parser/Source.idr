module Parser.Source

import Parser.Lexer.Source
import System.File

runParser : (str: String) -> IO ()
runParser str = pure ()

export parseFile : (fn : String) -> IO ()
parseFile fn = do
    Right str <- readFile fn
        | Left err => putStrLn $ show err
    runParser str
