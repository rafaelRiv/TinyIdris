module TTImp.Parser

import Core.TT
import Parser.Source
import public TTImp.TTImp

public export
FileName : Type
FileName = String

atom : FileName -> Rule RawImp
atom fname 
    =  do exactIdent "Type"
          pure IType
    <|> do symbol "_"
           pure Implicit
    <|> do x <- name
           pure (IVar x)

bindSymbol : Rule PiInfo
bindSymbol 
    = do symbol "->"
         pure Implicit

typeExpr : FileName -> IndentInfo -> Rule RawImp

dataDec : FileName -> IndentInfo -> Rule String
dataDec fname indents
   = do keyword "data"
        n <- name
        symbol ":"
        pure "Not Ready"

export
prog : FileName -> Rule (List RawImp)
prog fname = nonEmptyBlock (\x => atom fname)

