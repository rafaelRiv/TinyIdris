module Parser.Rule.Source

import public Parser.Lexer.Source
import public Parser.Rule.Common

%default total

public export 
Rule : Type -> Type
Rule = Rule Token

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
