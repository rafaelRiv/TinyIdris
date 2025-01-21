module Parser.Rule.Source

import public Parser.Lexer.Source
import public Parser.Rule.Common

%default total

public export 
Rule : Type -> Type
Rule = Rule Token

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


intLit : Rule Integer
intLit = terminal "Expected integer literal"
                  (\x => case tok x of
                          IntegerLit i => Just i
                          _ => Nothing)

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

public export
prog : Rule Integer
prog = intLit
