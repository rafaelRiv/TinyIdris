module Parser.Lexer.Source

import public Parser.Lexer.Common

data Token = 
  Comment | 
  Unrecognised String

rawToken : TokenMap Token
rawToken = 
  [
    (comment, const Comment)
  ]

Show Token where 
  show Comment = "comment"
  show (Unrecognised x) = "Unrecognised " ++ x

export
lexTo : String -> IO ()
lexTo str = do
  let (toks, (l,c,file)) = lexTo (const False) rawToken str
  putStrLn $ show toks

