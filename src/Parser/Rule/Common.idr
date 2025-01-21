module Parser.Rule.Common

import public Libraries.Text.Lexer
import public Libraries.Text.Parser

public export
Rule : Type -> Type -> Type
Rule token ty = Grammar (TokenData token) True ty

public export
EmptyRule : Type -> Type -> Type
EmptyRule token ty = Grammar (TokenData token) False ty

export
location : {token : _} -> EmptyRule token (Int,Int)
location
    = do tok <- peek
         pure (tok.line, tok.col)

export
column : {token : _} -> EmptyRule token Int
column
    = do (line,col) <- location
         pure col


