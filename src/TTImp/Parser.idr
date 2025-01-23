module TTImp.Parser

import Core.TT
import Parser.Source
import TTImp.TTImp

public export
FileName : Type
FileName = String

atom : FileName -> Rule String
atom fname 
    =  do exactIdent "Type"
          pure "Not Ready"

{-
typeExpr : FileName -> IndentInfo -> Rule RawImp
typeExpr fname indent 
      = do
        -}

dataDec : FileName -> IndentInfo -> Rule String
dataDec fname indents
   = do keyword "data"
        n <- name
        symbol ":"
        pure "Not Ready"

export
prog : Rule (List String)
prog = nonEmptyBlock (\x => atom "Test")

