module Parser.Lexer.Source

import public Parser.Lexer.Common

data Token = Comment String

rawToken : TokenMap Token
rawToken = 
  [
    (comment, Comment)
  ]

export
lexTo : String -> IO ()
lexTo str = 
    let lex = lexTo (const False) rawToken str
    in pure ()

