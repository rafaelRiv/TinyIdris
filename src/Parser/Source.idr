module Parser.Source

import public Parser.Lexer.Source
import public Parser.Rule.Source
import System.File

runParser : (str: String) -> IO ()
runParser str = do
  let (toks, (l,c,file)) = lexTo str
  let parsed = parse prog toks
  case parsed of
       Left err => putStrLn "Error"
       Right (ty, _) => putStrLn $ show (ty*2)


export parseFile : (fn : String) -> IO ()
parseFile fn = do
    Right str <- readFile fn
        | Left err => putStrLn $ show err
    runParser str
