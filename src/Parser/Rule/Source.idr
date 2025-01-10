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
public export
prog : Rule Integer
prog = intLit
