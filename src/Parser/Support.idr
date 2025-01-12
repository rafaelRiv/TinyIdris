module Parser.Support

import public Libraries.Text.Lexer
import public Libraries.Text.Parser

import Data.List

%default total

public export
data ParseError tok
  = ParseFail String (Maybe (Int,Int)) (List tok)
  | LexFail (Int, Int, String)

export
Show tok => Show (ParseError tok) where
  show (ParseFail err loc toks)
        = "Parse error: " ++ err ++ " (next tokens: "
            ++ show (take 10 toks) ++ ")"
  show (LexFail (c,l,str))
        = "Lex error at " ++ show (c,l) ++ "input: " ++ str

export
toGenericParsingError : ParsingError (TokenData token) -> ParseError token
toGenericParsingError (Error err []) = ParseFail err Nothing []
toGenericParsingError (Error err (t::ts)) = ParseFail err (Just (line t, col t)) (map tok (t::ts))

