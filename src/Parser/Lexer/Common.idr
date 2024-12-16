module Parser.Lexer.Common

import public Libraries.Text.Lexer

export
comment : Lexer
comment = is '-' <+> is '-'



