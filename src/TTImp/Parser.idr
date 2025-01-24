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

mutual
  appExpr : FileName -> IndentInfo -> Rule RawImp
  appExpr fname indents
    = do f <- simpleExpr fname indents
         pure f

  simpleExpr : FileName -> IndentInfo -> Rule RawImp
  simpleExpr fname indents
    = atom fname

  expr : FileName -> IndentInfo -> Rule RawImp
  expr = typeExpr

  typeExpr : FileName -> IndentInfo -> Rule RawImp
  typeExpr fname indents
        = do arg <- appExpr fname indents
             pure arg

forall_ : FileName -> IndentInfo -> Rule RawImp

binder : FileName -> IndentInfo -> Rule RawImp

dataDec : FileName -> IndentInfo -> Rule String
dataDec fname indents
   = do keyword "data"
        n <- name
        symbol ":"
        pure "Not Ready"

tyDec : FileName -> IndentInfo -> Rule ImpTy
tyDec fname indents
   = do n <- name
        symbol ":"
        ty <- expr fname indents
        atEnd indents
        pure (MkImpTy n ty)

export
prog : FileName -> Rule (List ImpTy)
prog fname = nonEmptyBlock (tyDec fname)

