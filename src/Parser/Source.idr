module Parser.Source

import public Parser.Lexer.Source
import public Parser.Rule.Source

import System.File
import Libraries.Utils.Either

runParser : Show ty => {e: _ } -> (str: String) -> Grammar (TokenData Token) e ty -> Either (ParseError Token) ty
runParser str p =
  do toks  <- mapError LexFail $ lexTo str
     parsed <- mapError toGenericParsingError $ parse p toks
     Right (fst parsed)

export parseFile : Show ty => (fn : String) -> Rule ty -> IO (Either (ParseError Token) ty)
parseFile fn p = do
    Right str <- readFile fn
        | Left err => pure (Left (FileFail err))
    pure (runParser str p)
