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

bindList : FileName -> IndentInfo -> 
          Rule (List (Name,RawImp))
bindList fname indents
    = sepBy1 (symbol ",")
             (do n <- unqualifiedName
                 ty <- pure Implicit
                 pure (UN n, ty))

typeExpr : FileName -> IndentInfo -> Rule RawImp

forall_ : FileName -> IndentInfo -> Rule RawImp

binder : FileName -> IndentInfo -> Rule RawImp

dataDec : FileName -> IndentInfo -> Rule String
dataDec fname indents
   = do keyword "data"
        n <- name
        symbol ":"
        pure "Not Ready"

export
prog : FileName -> Rule (List RawImp)
prog fname = nonEmptyBlock (\x => atom fname)

