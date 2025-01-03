module Parser.Lexer.Source

import public Parser.Lexer.Common

import Libraries.Utils.String

%default total

public export
data Token = 
  -- Literals
  CharLit String |
  DoubleLit Double |
  IntergerLit Integer |
  StringLit String |
  -- Identifiers

  -- Comments
  Comment | 
  -- Special
  Unrecognised String

Show Token where 
  -- Literals
  show (CharLit x) = "character " ++ show x 
  show (DoubleLit x) = "double " ++ show x
  show (IntergerLit x) = "integer " ++ show x
  show (StringLit x) = "string " ++ show x
  -- Identifiers
  -- Comments
  show Comment = "comment"
  -- Special
  show (Unrecognised x) = "Unrecognised " ++ x

doubleLit : Lexer
doubleLit
    = digits <+> is '.' <+> digits <+> opt
          (is 'e' <+> opt (is '-' <|> is '+') <+> digits)

rawToken : TokenMap Token
rawToken = 
  [
    (charLit, \x => CharLit (stripQuotes x)),
    (doubleLit, \x => DoubleLit (cast x)),
    --(integerLit, \x => IntegerLit (stripQuotes x))
    --(stringLit, \x => StringLit (stripQuotes x))
    (comment, const Comment)
  ]

export
lexTo : String -> IO ()
lexTo str = do
  let (toks, (l,c,file)) = lexTo (const False) rawToken str
  putStrLn $ show toks

