module Parser.Lexer.Package

import public Parser.Lexer.Common

import Data.List1
import Data.List
import Data.String
import Libraries.Data.String.Extra
import Libraries.Utils.String

%default total

public export
data Token 
  = Comment String
  | Equals
  | DotSepIdent (List1 String)
  | Separator
  | Space
  | StringLit String

Show Token where 
  show (Comment x) = "Comment " ++ x
  show Equals = "Equals"
  show (DotSepIdent dsid)= "DotSepIdentifier: " ++ dotSep (forget dsid)
  show Separator = "Separator"
  show Space = "Space"
  show (StringLit s) = "StringLit: " ++ s

equals : Lexer
equals = is '='

separator: Lexer
separator = is ','

rawToken : TokenMap Token
rawToken = 
  [ (comment, Comment),
    (equals, const Equals),
    (namespacedIdent, DotSepIdent . splitNamespace),
    (identAllowDashes, DotSepIdent . pure),
    (separator, const Separator),
    (spacesOrNewlines, const Space),
    (stringLit, \s => StringLit (stripQuotes s))
  ]

  where
    splitNamespace : String -> List1 String
    splitNamespace = Data.String.split (== '.')

export
lex : String -> IO ()
lex str = do
  let (toks, (l,c,file)) = lexTo (const False) rawToken str
  putStrLn $ show toks

