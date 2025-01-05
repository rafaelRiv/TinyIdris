module Parser.Lexer.Source

import public Parser.Lexer.Common
import Libraries.Utils.String
import Libraries.Utils.Hex
import Libraries.Utils.Octal

import Data.String

%default total

public export
data Token = 
  -- Literals
  CharLit String |
  DoubleLit Double |
  IntegerLit Integer |
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
  show (IntegerLit x) = "integer " ++ show x
  show (StringLit x) = "string " ++ show x
  -- Identifiers
  -- Comments
  show Comment = "comment"
  -- Special
  show (Unrecognised x) = "Unrecognised " ++ x

fromHexLit : String -> Integer
fromHexLit str
  = if length str <= 2
       then 0
       else let num = assert_total (strTail (strTail str)) in
            case fromHex (reverse num) of
                 Nothing => 0
                 Just n => cast n

fromOctLit : String -> Integer
fromOctLit str
  = if length str <= 2
       then 0
       else let num = assert_total (strTail (strTail str)) in
            case fromOct (reverse num) of
                 Nothing => 0
                 Just n => cast n

doubleLit : Lexer
doubleLit
    = digits <+> is '.' <+> digits <+> opt
          (is 'e' <+> opt (is '-' <|> is '+') <+> digits)

rawToken : TokenMap Token
rawToken = 
  [
    (charLit, \x => CharLit (stripQuotes x)),
    (doubleLit, \x => DoubleLit (cast x)),
    (hexLit, \x => IntegerLit (fromHexLit x)),
    (octLit, \x => IntegerLit (fromOctLit x)),
    (digits, \x => IntegerLit (cast x)),
    (stringLit, \x => StringLit (stripQuotes x)),
    (comment, const Comment)
  ]

export
lexTo : String -> IO ()
lexTo str = do
  let (toks, (l,c,file)) = lexTo (const False) rawToken str
  putStrLn $ show toks

