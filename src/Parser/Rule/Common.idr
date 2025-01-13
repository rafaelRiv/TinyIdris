module Parser.Rule.Common

import public Libraries.Text.Lexer
import public Libraries.Text.Parser

public export
Rule : Type -> Type -> Type
Rule token ty = Grammar (TokenData token) True ty

