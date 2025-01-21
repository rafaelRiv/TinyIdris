module Parser.Rule.Source

import public Parser.Lexer.Source
import public Parser.Rule.Common

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

intLit : Rule Integer
intLit = terminal "Expected integer literal"
                  (\x => case tok x of
                          IntegerLit i => Just i
                          _ => Nothing)

symbol : String -> Rule ()
symbol req
    = terminal ("Expect '" ++ req ++ "'")
               (\x => case tok x of
                          Symbol s => if s == req then Just ()
                                                  else Nothing
                          _ => Nothing)
export
IndentInfo : Type
IndentInfo = Int

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


reservedNames : List String
reservedNames 
    = ["Type", "Int", "Integer", "Bits8", "Bits16", "Bits64",
       "String", "Char", "Double", "Lazy", "Inf", "Force", "Delay"]

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


nonEmptyBlock : (IndentInfo -> Rule ty) -> Rule (List ty)
nonEmptyBlock item
  = do symbol "{"
       commit
       pure []


public export
prog : Rule Integer
prog = intLit
