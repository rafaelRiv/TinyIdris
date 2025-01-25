module Parser.Rule.Source

import Parser.Lexer.Source
import public Parser.Rule.Common
import public Parser.Support

import Core.TT

%default total

public export 
Rule : Type -> Type
Rule = Rule Token

public export
SourceEmptyRule : Type -> Type
SourceEmptyRule = EmptyRule Token

export
eoi : SourceEmptyRule ()
eoi
  = do nextIs "Expected end of input" (isEOI . tok)
       pure ()
  where
    isEOI : Token -> Bool
    isEOI EndInput = True
    isEOI _ = False

export
intLit : Rule Integer
intLit = terminal "Expected integer literal"
                  (\x => case tok x of
                          IntegerLit i => Just i
                          _ => Nothing)

export
symbol : String -> Rule ()
symbol req
    = terminal ("Expect '" ++ req ++ "'")
               (\x => case tok x of
                          Symbol s => if s == req then Just ()
                                                  else Nothing
                          _ => Nothing)
export
keyword : String -> Rule ()
keyword req
    = terminal ("Expect '" ++ req ++ "'")
               (\x => case tok x of
                          Keyword s => if s == req then Just ()
                                                   else Nothing
                          _ => Nothing)

export
exactIdent : String -> Rule ()
exactIdent req
    = terminal ("Expect " ++ req)
               (\x => case tok x of
                          Ident s => if s == req then Just ()
                                                 else Nothing
                          _ => Nothing)

export
identPart : Rule String
identPart
    = terminal ("Expect name")
               (\x => case tok x of
                          Ident str => Just str
                          _ => Nothing)
export
unqualifiedName : Rule String
unqualifiedName = identPart

export
reservedNames : List String
reservedNames 
    = ["Type", "Int", "Integer", "Bits8", "Bits16", "Bits64",
       "String", "Char", "Double", "Lazy", "Inf", "Force", "Delay"]

export
name : Rule Name
name = do n <- unqualifiedName
          pure (UN n)

export
IndentInfo : Type
IndentInfo = Int

continueF : SourceEmptyRule () -> (indent : IndentInfo) -> SourceEmptyRule ()
continueF err indent 
    = do eoi; err
  <|> do keyword "where"; err
  <|> do col <- Common.column
         if col <= indent
            then err
            else pure ()

export
continue : (indent : IndentInfo) -> SourceEmptyRule ()
continue = continueF (fail "Unexpected end of expression")


data ValidIndent = 
  ||| in {}, enties can begin in any column
  AnyIndent |
  ||| Entry must begin in a specific column
  AtPos Int |
  ||| Enty can begin in this column or later
  AfterPos Int |
  ||| Block is finished
  EndOfBlock

Show ValidIndent where
  show AnyIndent = "[any]"
  show (AtPos i) = "[col " ++ show i ++ "]"
  show (AfterPos i) = "[after " ++ show i ++ "]"
  show EndOfBlock = "[EOB]"

checkValid : ValidIndent -> Int -> SourceEmptyRule ()
checkValid AnyIndent c = pure ()
checkValid (AtPos x) c = if c == x
                            then pure ()
                            else fail "Invalid indentation"
checkValid (AfterPos x) c = if c >= x
                              then pure ()
                              else fail "Invalid indentation"
checkValid EndOfBlock c = fail "End of block"


||| Any token which indicates the end of a statement/block
isTerminator : Token -> Bool
isTerminator (Symbol ",") = True
isTerminator (Symbol "]") = True
isTerminator (Symbol ";") = True
isTerminator (Symbol "}") = True
isTerminator (Symbol ")") = True
isTerminator (Symbol "|") = True
isTerminator (Symbol "in") = True
isTerminator (Symbol "then") = True
isTerminator (Symbol "else") = True
isTerminator (Symbol "where") = True
isTerminator EndInput = True
isTerminator _ = False

export
atEnd : (indent: IndentInfo) -> SourceEmptyRule ()
atEnd indent
  = eoi
  <|> do nextIs "Expected end of block" (isTerminator . tok)
         pure ()
  <|> do col <- Common.column
         if (col <= indent)
            then pure ()
            else fail "Not the end of a block entry"

-- Parse a terminator, return where the next block entry
-- must start, given where the current block enty started
terminator : ValidIndent -> Int -> SourceEmptyRule ValidIndent
terminator valid laststart
      = do eoi
           pure EndOfBlock
    <|> do symbol ";"
           pure (afterSemi valid)
    <|> do col <- column
           afterDedent valid col
    <|> pure EndOfBlock

    where
      afterSemi : ValidIndent -> ValidIndent
      afterSemi AnyIndent = AnyIndent
      afterSemi (AtPos c) = AfterPos c
      afterSemi (AfterPos c) = AfterPos c
      afterSemi EndOfBlock = EndOfBlock

      afterDedent : ValidIndent -> Int -> SourceEmptyRule ValidIndent
      afterDedent AnyIndent col
          = if col <= laststart
               then pure AnyIndent
               else fail "Not the end of a block entry"
      afterDedent (AfterPos c) col
          = if col <= laststart
               then pure AnyIndent
               else fail "Not the end of a block entry"
      afterDedent (AtPos c) col
          = if col <= laststart
               then pure AnyIndent
               else fail "Not the end of a block entry"
      afterDedent EndOfBlock col = pure EndOfBlock
      

blockEntry : ValidIndent -> (IndentInfo -> Rule ty) -> Rule (ty,ValidIndent)
blockEntry valid rule
    = do col <- column
         checkValid valid col
         p <- rule col
         valid' <- terminator valid col
         pure (p,valid')

blockEntries : ValidIndent -> (IndentInfo -> Rule ty) -> SourceEmptyRule (List ty)
blockEntries valid rule
    = do eoi; pure []
    <|> do res <- blockEntry valid rule
           ts <- blockEntries (snd res) rule
           pure (fst res :: ts)
    <|> pure []

export
nonEmptyBlock : (IndentInfo -> Rule ty) -> Rule (List ty)
nonEmptyBlock item
    = do symbol "{"
         commit
         res <- blockEntry AnyIndent item
         ps <- blockEntries (snd res) item
         symbol "}"
         pure (fst res :: ps)
  <|> do col <- column
         res <- blockEntry (AtPos col) item
         ps <- blockEntries (snd res) item
         pure (fst res :: ps)



