module Libraries.Text.Lexer

import public Libraries.Text.Lexer.Core

export
||| Recognise any character
any : Lexer
any = pred (const True)

export
is : (x: Char) -> Lexer
is x = pred (==x)

