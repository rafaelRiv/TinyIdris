module Parser.Source

import public Parser.Lexer.Source
import public Parser.Rule.Source
import Parser.Support

import System.File
import Libraries.Utils.Either

runParser : (str: String) -> IO ()
runParser str = do
  let (toks, (l,c,file)) = lexTo str
  let parsed = mapError toGenericParsingError $ parse prog toks
  case parsed of
       Left err => putStrLn $ show err
       Right (ty, _) => putStrLn $ show (ty*2)


export parseFile : (fn : String) -> IO ()
parseFile fn = do
    Right str <- readFile fn
        | Left err => putStrLn $ show err
    runParser str
