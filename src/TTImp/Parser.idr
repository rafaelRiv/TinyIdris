module TTImp.Parser

import Core.TT
import public Libraries.Text.Parser
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

mutual
  appExpr : FileName -> IndentInfo -> Rule RawImp
  appExpr fname indents
    = do f <- simpleExpr fname indents
         pure f

  simpleExpr : FileName -> IndentInfo -> Rule RawImp
  simpleExpr fname indents
    = atom fname <|> binder fname indents

  expr : FileName -> IndentInfo -> Rule RawImp
  expr = typeExpr

  typeExpr : FileName -> IndentInfo -> Rule RawImp
  typeExpr fname indents
        = do arg <- appExpr fname indents
             (do continue indents
                 rest <- some (do exp <- bindSymbol
                                  op <- appExpr fname indents
                                  pure (exp,op))
                 pure (mkPi arg rest))
              <|> pure arg

        where
          mkPi : RawImp -> List (PiInfo, RawImp) -> RawImp
          mkPi arg [] = arg
          mkPi arg ((exp, a) :: as)
                = IPi exp Nothing arg (mkPi a as)

  bindList : FileName -> IndentInfo -> 
             Rule (List (Name,RawImp))
  bindList fname indents
      = sepBy1 (symbol ",")
               (do n <- unqualifiedName
                   ty <- option
                            Implicit
                            (do symbol ":"
                                appExpr fname indents)
                   pure (UN n, ty))

  forall_ : FileName -> IndentInfo -> Rule RawImp

  pat : FileName -> IndentInfo -> Rule RawImp
  pat fname indents 
      = do keyword "pat"
           binders <- bindList fname indents
           symbol "=>"
           mustContinue indents Nothing
           scope <- expr fname indents
           end <- location
           pure (bindAll binders scope)

      where
        bindAll : List (Name, RawImp) -> RawImp -> RawImp
        bindAll [] scope = scope
        bindAll ((n,ty) :: rest) scope
              = IPatvar n ty (bindAll rest scope)


  binder : FileName -> IndentInfo -> Rule RawImp
  binder fname indents
      = pat fname indents

tyDec : FileName -> IndentInfo -> Rule ImpTy
tyDec fname indents
   = do n <- name
        symbol ":"
        ty <- expr fname indents
        atEnd indents
        pure (MkImpTy n ty)

dataDec : FileName -> IndentInfo -> Rule ImpData
dataDec fname indents
   = do keyword "data"
        n <- name
        symbol ":"
        ty <- expr fname indents
        keyword "where"
        cs <- block (tyDec fname)
        pure (MkImpData n ty cs)

topDecl : FileName -> IndentInfo -> Rule ImpDecl
topDecl fname indents
    = do dat <- dataDec fname indents
         pure (IData dat)
  <|> do claim <- tyDec fname indents
         pure (IClaim claim)

export
prog : FileName -> Rule (List ImpDecl)
prog fname = nonEmptyBlock (topDecl fname)

