module Parser.Source

import public Parser.Lexer.Source
import public Parser.Rule.Source

import System.File
import Libraries.Utils.Either

runParser : Show ty => {e: _ } -> (str: String) -> Grammar (TokenData Token) e ty -> IO ()
runParser str p = do
  let (toks, (l,c,file)) = lexTo str
  let parsed = mapError toGenericParsingError $ parse p toks
  case parsed of
       Left err => putStrLn $ show err
       Right (ty, _) => putStrLn $ show ty

export parseFile : Show ty => (fn : String) -> Rule ty -> IO ()
parseFile fn p = do
    Right str <- readFile fn
        | Left err => putStrLn $ show err
    runParser str p
