module Parser.Lexer.Source

import public Parser.Lexer.Common
import Libraries.Utils.String
import Libraries.Utils.Hex
import Libraries.Utils.Octal

import Data.String
import Libraries.Data.String.Extra

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
  Comment String |
  DocComment String |
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
  show (Comment x) = "comment" ++ x
  show (DocComment x) = "DocComment" ++ x
  -- Special
  show (Unrecognised x) = "Unrecognised " ++ x

mutual
  ||| The mutually defined functions represent different states in a
  ||| small automaton.
  ||| `toEndComment` is the default state and it will munch through
  ||| the input until we detect a special character (a dash, an
  ||| opening brace, or a double quote) and then switch to the
  ||| appropriate state.
  toEndComment : (k : Nat) -> Recognise (k /= 0)
  toEndComment Z = empty
  toEndComment (S k)
               = some (pred (\c => c /= '-' && c /= '{' && c /= '"'))
                        <+> toEndComment (S k)
             <|> is '{' <+> singleBrace k
             <|> is '-' <+> singleDash k
             <|> stringLit <+> toEndComment (S k)

  ||| After reading a single brace, we may either finish reading an
  ||| opening delimiter or ignore it (e.g. it could be an implicit
  ||| binder).
  singleBrace : (k : Nat) -> Lexer
  singleBrace k
     =  is '-' <+> many (is '-')    -- opening delimiter
               <+> singleDash (S k) -- handles the {----} special case
    <|> toEndComment (S k)          -- not a valid comment

  ||| After reading a single dash, we may either find another one,
  ||| meaning we may have started reading a line comment, or find
  ||| a closing brace meaning we have found a closing delimiter.
  singleDash : (k : Nat) -> Lexer
  singleDash k
     =  is '-' <+> doubleDash k    -- comment or closing delimiter
    <|> is '}' <+> toEndComment k  -- closing delimiter
    <|> toEndComment (S k)         -- not a valid comment

  ||| After reading a double dash, we are potentially reading a line
  ||| comment unless the series of uninterrupted dashes is ended with
  ||| a closing brace in which case it is a closing delimiter.
  doubleDash : (k : Nat) -> Lexer
  doubleDash k = many (is '-') <+> choice {t = List} -- absorb all dashes
    [ is '}' <+> toEndComment k                      -- closing delimiter
    , many (isNot '\n') <+> toEndComment (S k)       -- line comment
    ]

blockComment : Lexer
blockComment = is '{' <+> is '-' <+> toEndComment 1

docComment : Lexer
docComment = is '|' <+> is '|' <+> is '|' <+> many (isNot '\n')

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
    (comment, Comment),
    (blockComment, Comment),
    (docComment, DocComment . drop 3),
    (doubleLit, \x => DoubleLit (cast x)),
    (hexLit, \x => IntegerLit (fromHexLit x)),
    (octLit, \x => IntegerLit (fromOctLit x)),
    (digits, \x => IntegerLit (cast x)),
    (stringLit, \x => StringLit (stripQuotes x)),
    (charLit, \x => CharLit (stripQuotes x)),
    (space, Comment)
  ]

export
lexTo : String -> IO ()
lexTo str = do
  let (toks, (l,c,file)) = lexTo (const False) rawToken str
  putStrLn $ show toks

