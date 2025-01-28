module Parser.Lexer.Source

import public Parser.Lexer.Common

import Data.List1
import Data.List
import Data.String
import Libraries.Data.String.Extra

import Libraries.Utils.String
import Libraries.Utils.Hex
import Libraries.Utils.Octal

%default total

public export
data Token = 
  -- Literals
  CharLit String |
  DoubleLit Double |
  IntegerLit Integer |
  StringLit String |
  -- Identifiers
  HoleIdent String |
  Ident String |
  DotSepIdent (List1 String) | -- ident.ident
  DotIdent String |            -- .ident
  Symbol String |
  -- Comments
  Comment String |
  DocComment String |
  -- Special
  CGDirective String |
  EndInput |
  Keyword String |
  Pragma String |
  Unrecognised String

public export
Show Token where 
  -- Literals
  show (CharLit x) = "character " ++ show x 
  show (DoubleLit x) = "double " ++ show x
  show (IntegerLit x) = "integer " ++ show x
  show (StringLit x) = "string " ++ show x
  -- Identifiers
  show (HoleIdent x) = "hole identifier " ++ show x 
  show (Ident x) = "identifier " ++ x
  show (DotSepIdent xs) = "namespaced identifier " ++ dotSep (forget $ reverse xs)
  show (DotIdent x) = "dot identifier " ++ show x 
  show (Symbol x) = "symbol " ++ x 
  -- Comments
  show (Comment x) = "comment " ++ x
  show (DocComment x) = "doc comment " ++ x
  -- Special
  show (CGDirective x) = "CGDirective " ++ x
  show EndInput = "end of input"
  show (Keyword x) = "keyword " ++ x
  show (Pragma x) = "pragma " ++ x
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

holeIdent : Lexer
holeIdent = is '?' <+> identNormal

dotIdent : Lexer
dotIdent = is '.' <+> identNormal

pragma : Lexer
pragma = is '%' <+> identNormal

doubleLit : Lexer
doubleLit
  = digits <+> is '.' <+> digits <+> opt
        (is 'e' <+> opt (is '-' <|> is '+') <+> digits)

-- Do this as an entire token, because the contents will be processed by
-- a specific back end
cgDirective : Lexer
cgDirective
  = exact "%cg" <+>
    ((some space <+>
        some (pred isAlphaNum) <+> many space <+>
        is '{' <+> many (isNot '}') <+>
        is '}')
      <+> many (isNot '\n'))

mkDirective : String -> Token
mkDirective str = CGDirective (trim (substr 3 (length str) str))

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

keywords : List String
keywords = ["data", "module", "where", "let", "pat", "in", "do", "record",
            "auto", "default", "implicit", "mutual", "namespace",
            "parameters", "with", "impossible", "case", "of",
            "if", "then", "else", "forall", "rewrite",
            "using", "interface", "implementation", "open", "import",
            "public", "export", "private",
            "infixl", "infixr", "infix", "prefix",
            "total", "partial", "covering"]

symbols : List String
symbols
  = [".(",
     "@{",
     "[|", "|]|",
     "(", ")", "{", "}}", "}", "[", "]", ",", ";", "_",
     "`(", "`{{", "`[", "`"]

isOpChar : Char -> Bool
isOpChar c = c `elem` (unpack ":!#$%&*+./<=>?@\\^|-~")

validSymbol : Lexer
validSymbol = some (pred isOpChar)

rawToken : TokenMap Token
rawToken = 
  [ (comment, Comment),
    (blockComment, Comment),
    (docComment, DocComment . drop 3),
    (cgDirective, mkDirective),
    (holeIdent, \x => HoleIdent (assert_total (strTail x)))] ++
  map (\x => (exact x, Symbol)) symbols ++
  [ (doubleLit, \x => DoubleLit (cast x)),
    (hexLit, \x => IntegerLit (fromHexLit x)),
    (octLit, \x => IntegerLit (fromOctLit x)),
    (digits, \x => IntegerLit (cast x)),
    (stringLit, \x => StringLit (stripQuotes x)),
    (charLit, \x => CharLit (stripQuotes x)),
    (dotIdent, \x => DotIdent (assert_total (strTail x))),
    (namespaceIdent, parseNamespace),
    (identNormal, parseIdent),
    (pragma, \x => Pragma (assert_total (strTail x))),
    (space, Comment),
    (validSymbol, Symbol),
    (symbol, Unrecognised)
  ]

  where
    parseIdent : String -> Token
    parseIdent x = if x `elem` keywords then Keyword x else Ident x

    parseNamespace : String -> Token
    parseNamespace ns = case List1.reverse . split (== '.') $ ns of
                             (ident ::: []) => parseIdent ident
                             ns => DotSepIdent ns

export
lexTo : String -> ((List (TokenData Token)), (Int,Int,String))
lexTo str = let (toks, (l,c,file))  = lexTo (const False) rawToken str
            in ((filter notComment toks) ++ [MkToken l c l c EndInput], (l,c,file))

    where
      notComment : TokenData Token -> Bool
      notComment t = case tok t of
                        Comment _ => False
                        _ => True

